const std = @import("std");
const ast = @import("pegz_common").ast;
const token = @import("token.zig");
const blacklisted_idents = @import("token.zig").BlacklistedIdents;

const Tid = token.Tid;
const Token = token.Token;

const ScannerError = error{
    ScannerError,
    EndOfStream,
    UnexpectedEOF,
    InvalidCharacter,
};

/// Error handler function type
pub const ErrorFn = *const fn (pos: ast.Pos, err_msg: []const u8) void;

/// Scanner tokenizes an input source for PEG grammar.
pub const Scanner = struct {
    allocator: std.mem.Allocator,
    reader: *std.io.Reader,
    errh: ?ErrorFn,

    eof: bool,
    cur: u21, // Unicode code point
    cpos: ast.Pos,
    cw: usize, // width of current character in bytes

    tok_buffer: std.ArrayList(u8),
    blacklisted: blacklisted_idents.HashMap,

    pub fn init(allocator: std.mem.Allocator, filename: []const u8, reader: *std.io.Reader, errh: ?ErrorFn) !Scanner {
        const tok_buffer: std.ArrayList(u8) = .empty;
        var scanner = Scanner{
            .allocator = allocator,
            .reader = reader,
            .errh = errh,
            .eof = false,
            .cur = std.math.maxInt(u21),
            .cpos = ast.Pos{
                .filename = filename,
                .line = 1,
                .column = 1,
                .offset = 0,
            },
            .cw = 0,
            .tok_buffer = tok_buffer,
            .blacklisted = blacklisted_idents.init(allocator),
        };

        _ = scanner.blacklisted; // Initialize with default keywords

        // Move to first rune
        scanner.read();

        return scanner;
    }

    /// Scan returns the next token, along with a boolean indicating if EOF was
    /// reached (false means no more tokens).
    pub fn scan(self: *Scanner) !Token {
        var result = Token{
            .id = .invalid,
            .lit = "",
            .pos = self.cpos,
        };

        if (!self.eof and self.cur == std.math.maxInt(u21)) {
            // move to first rune
            self.read();
        }

        self.skipWhitespace();
        result.pos = self.cpos;

        // first switch cases all position scanner on the next rune
        // by their calls to scan*
        if (self.eof) {
            result.id = .eof;
        } else if (isLetter(self.cur)) {
            result.id = .ident;
            result.lit = try self.scanIdentifier();

            if (self.blacklisted.contains(result.lit)) {
                self.errorpf(result.pos, "illegal identifier {s}", .{result.lit});
            }
        } else if (isRuleDefStart(self.cur)) {
            result.id = .ruledef;
            result.lit = try self.scanRuleDef();
        } else if (self.cur == '\'') {
            result.id = .char;
            result.lit = try self.scanChar();
        } else if (self.cur == '"') {
            result.id = .str;
            result.lit = try self.scanString();
        } else if (self.cur == '`') {
            result.id = .rstr;
            result.lit = try self.scanRawString();
        } else if (self.cur == '[') {
            result.id = .class;
            result.lit = try self.scanClass();
        } else if (self.cur == '{') {
            result.id = .code;
            result.lit = try self.scanCode();
        } else {
            const r = self.cur;
            self.read();
            switch (r) {
                '/' => {
                    if (self.cur == '*' or self.cur == '/') {
                        const comment = try self.scanComment();
                        result.id = comment.id;
                        result.lit = comment.lit;
                    } else {
                        result.id = @enumFromInt(r);
                        result.lit = try self.allocator.dupe(u8, &[_]u8{@intCast(r)});
                    }
                },
                ':', ';', '(', ')', '.', '&', '!', '?', '+', '*', '\n' => {
                    result.id = @enumFromInt(r);
                    result.lit = try self.allocator.dupe(u8, &[_]u8{@intCast(r)});
                },
                else => {
                    self.errorf("invalid character {u}", .{r});
                    result.id = .invalid;
                    result.lit = try self.allocator.dupe(u8, &[_]u8{@intCast(r)});
                },
            }
        }

        return result;
    }

    fn scanIdentifier(self: *Scanner) ![]const u8 {
        self.tok_buffer.clearAndFree(self.allocator);

        while (isLetter(self.cur) or isDigit(self.cur)) {
            var buf: [4]u8 = undefined;
            const len = try std.unicode.utf8Encode(self.cur, &buf);
            try self.tok_buffer.appendSlice(self.allocator, buf[0..len]);
            self.read();
        }

        return try self.tok_buffer.toOwnedSlice(self.allocator);
    }

    fn scanComment(self: *Scanner) !struct { id: Tid, lit: []const u8 } {
        self.tok_buffer.clearAndFree(self.allocator);
        var buf: [4]u8 = undefined;
        _ = try std.unicode.utf8Encode('/', &buf);
        try self.tok_buffer.appendSlice(self.allocator, &buf); // initial '/' already consumed

        var multiline = false;
        switch (self.cur) {
            '*' => multiline = true,
            '\n', std.math.maxInt(u21) => {
                self.errorf("comment not terminated", .{});
                return .{ .id = .lcomment, .lit = try self.tok_buffer.toOwnedSlice(self.allocator) };
            },
            else => {},
        }

        var closing = false;
        while (true) {
            _ = try std.unicode.utf8Encode(self.cur, &buf);
            try self.tok_buffer.appendSlice(self.allocator, &buf);
            self.read();

            switch (self.cur) {
                '\n' => {
                    if (!multiline) {
                        return .{ .id = .lcomment, .lit = try self.tok_buffer.toOwnedSlice(self.allocator) };
                    }
                },
                std.math.maxInt(u21) => {
                    if (multiline) {
                        self.errorf("comment not terminated", .{});
                        return .{ .id = .mlcomment, .lit = try self.tok_buffer.toOwnedSlice(self.allocator) };
                    }
                    return .{ .id = .lcomment, .lit = try self.tok_buffer.toOwnedSlice(self.allocator) };
                },
                '*' => {
                    if (multiline) {
                        closing = true;
                    }
                },
                '/' => {
                    if (closing) {
                        _ = try std.unicode.utf8Encode(self.cur, &buf);
                        try self.tok_buffer.appendSlice(self.allocator, &buf);
                        self.read();
                        return .{ .id = .mlcomment, .lit = try self.tok_buffer.toOwnedSlice(self.allocator) };
                    }
                },
                else => {},
            }
        }
    }

    fn scanCode(self: *Scanner) ![]const u8 {
        self.tok_buffer.clearAndFree(self.allocator);
        var buf: [4]u8 = undefined;
        _ = try std.unicode.utf8Encode(self.cur, &buf);
        try self.tok_buffer.appendSlice(self.allocator, &buf);

        var depth: usize = 1;
        while (true) {
            self.read();
            _ = try std.unicode.utf8Encode(self.cur, &buf);
            try self.tok_buffer.appendSlice(self.allocator, &buf);

            switch (self.cur) {
                std.math.maxInt(u21) => {
                    self.errorf("code block not terminated", .{});
                    return self.tok_buffer.toOwnedSlice(self.allocator);
                },
                '{' => depth += 1,
                '}' => {
                    depth -= 1;
                    if (depth == 0) {
                        self.read();
                        return self.tok_buffer.toOwnedSlice(self.allocator);
                    }
                },
                else => {},
            }
        }
    }

    fn scanEscape(self: *Scanner, quote: u21) !bool {
        // scanEscape is always called as part of a greater token, so do not
        // reset tok_buffer, and write cur before calling read.
        var buf: [4]u8 = undefined;
        _ = try std.unicode.utf8Encode(self.cur, &buf);
        try self.tok_buffer.appendSlice(self.allocator, &buf);

        var n: usize = undefined;
        var base: u8 = undefined;
        var max: u32 = undefined;
        var unicode_class = false;

        self.read();
        if (self.cur == quote) {
            var buf2: [4]u8 = undefined;
            _ = try std.unicode.utf8Encode(self.cur, &buf2);
            try self.tok_buffer.appendSlice(self.allocator, &buf2);
            return true;
        }
        switch (self.cur) {
            'a', 'b', 'f', 'n', 'r', 't', 'v', '\\' => {
                var buf2: [4]u8 = undefined;
                _ = try std.unicode.utf8Encode(self.cur, &buf2);
                try self.tok_buffer.appendSlice(self.allocator, &buf2);
                return true;
            },
            '0', '1', '2', '3', '4', '5', '6', '7' => {
                n = 3;
                base = 8;
                max = 255;
            },
            'x' => {
                _ = try std.unicode.utf8Encode(self.cur, &buf);
                try self.tok_buffer.appendSlice(self.allocator, &buf);
                self.read();
                n = 2;
                base = 16;
                max = 255;
            },
            'u' => {
                _ = try std.unicode.utf8Encode(self.cur, &buf);
                try self.tok_buffer.appendSlice(self.allocator, &buf);
                self.read();
                n = 4;
                base = 16;
                max = std.math.maxInt(u21);
            },
            'U' => {
                _ = try std.unicode.utf8Encode(self.cur, &buf);
                try self.tok_buffer.appendSlice(self.allocator, &buf);
                self.read();
                n = 8;
                base = 16;
                max = std.math.maxInt(u21);
            },
            'p' => {
                // unicode character class, only valid if quote is ']'
                if (quote == ']') {
                    _ = try std.unicode.utf8Encode(self.cur, &buf);
                    try self.tok_buffer.appendSlice(self.allocator, &buf);
                    unicode_class = true;
                    self.read();
                } else {
                    // fallthrough to default case
                    _ = try std.unicode.utf8Encode(self.cur, &buf);
                    try self.tok_buffer.appendSlice(self.allocator, &buf);
                    const msg = "unknown escape sequence";
                    if (self.cur == std.math.maxInt(u21) or self.cur == '\n') {
                        self.errorf("{s}", .{msg});
                    } else {
                        self.errorf("{s}", .{msg});
                        self.read();
                    }
                    return false;
                }
            },
            else => {
                _ = try std.unicode.utf8Encode(self.cur, &buf);
                try self.tok_buffer.appendSlice(self.allocator, &buf);
                const msg = "unknown escape sequence";
                if (self.cur == std.math.maxInt(u21) or self.cur == '\n') {
                    self.errorf("escape sequence not terminated", .{});
                } else {
                    self.errorf("{s}", .{msg});
                    self.read();
                }
                return false;
            },
        }

        if (unicode_class) {
            switch (self.cur) {
                '\n', std.math.maxInt(u21) => {
                    self.errorf("escape sequence not terminated", .{});
                    return false;
                },
                '{' => {
                    // unicode class name, read until '}'
                    var cnt: usize = 0;
                    while (true) {
                        _ = try std.unicode.utf8Encode(self.cur, &buf);
                        try self.tok_buffer.appendSlice(self.allocator, &buf);
                        self.read();
                        cnt += 1;

                        switch (self.cur) {
                            '\n', std.math.maxInt(u21) => {
                                self.errorf("escape sequence not terminated", .{});
                                return false;
                            },
                            '}' => {
                                if (cnt < 2) {
                                    self.errorf("empty Unicode character class escape sequence", .{});
                                }
                                _ = try std.unicode.utf8Encode(self.cur, &buf);
                                try self.tok_buffer.appendSlice(self.allocator, &buf);
                                return true;
                            },
                            else => {},
                        }
                    }
                },
                else => {
                    // single letter class
                    _ = try std.unicode.utf8Encode(self.cur, &buf);
                    try self.tok_buffer.appendSlice(self.allocator, &buf);
                    return true;
                },
            }
        }

        var x: u32 = 0;
        while (n > 0) {
            _ = try std.unicode.utf8Encode(self.cur, &buf);
            try self.tok_buffer.appendSlice(self.allocator, &buf);
            const d = digitVal(self.cur);
            if (d >= base) {
                const msg = std.fmt.allocPrint(self.allocator, "illegal character {u} in escape sequence", .{self.cur}) catch "illegal character in escape sequence";
                if (self.cur == std.math.maxInt(u21) or self.cur == '\n') {
                    self.errorf("escape sequence not terminated", .{});
                } else {
                    self.errorf("{s}", .{msg});
                    self.read();
                }
                self.allocator.free(msg);
                return false;
            }
            x = x * base + d;
            n -= 1;

            if (n > 0) {
                self.read();
            }
        }

        if (x > max or (0xd800 <= x and x <= 0xe000)) {
            self.errorf("escape sequence is invalid Unicode code point", .{});
            self.read();
            return false;
        }
        return true;
    }

    fn scanClass(self: *Scanner) ![]const u8 {
        self.tok_buffer.clearAndFree(self.allocator);
        var buf: [4]u8 = undefined;
        _ = try std.unicode.utf8Encode(self.cur, &buf);
        try self.tok_buffer.appendSlice(self.allocator, &buf); // opening '['

        var noread = false;
        while (true) {
            if (!noread) {
                self.read();
            }
            noread = false;

            switch (self.cur) {
                '\\' => {
                    noread = !try self.scanEscape(']');
                },
                '\n', std.math.maxInt(u21) => {
                    // \n not consumed
                    self.errorf("character class not terminated", .{});
                    return self.tok_buffer.toOwnedSlice(self.allocator);
                },
                ']' => {
                    _ = try std.unicode.utf8Encode(self.cur, &buf);
                    try self.tok_buffer.appendSlice(self.allocator, &buf);
                    self.read();
                    // can have an optional "i" ignore case suffix
                    if (self.cur == 'i') {
                        _ = try std.unicode.utf8Encode(self.cur, &buf);
                        try self.tok_buffer.appendSlice(self.allocator, &buf);
                        self.read();
                    }
                    return self.tok_buffer.toOwnedSlice(self.allocator);
                },
                else => {
                    _ = try std.unicode.utf8Encode(self.cur, &buf);
                    try self.tok_buffer.appendSlice(self.allocator, &buf);
                },
            }
        }
    }

    fn scanRawString(self: *Scanner) ![]const u8 {
        self.tok_buffer.clearAndFree(self.allocator);
        var buf: [4]u8 = undefined;
        _ = try std.unicode.utf8Encode(self.cur, &buf);
        try self.tok_buffer.appendSlice(self.allocator, &buf); // opening '`'

        var has_cr = false;
        while (true) {
            self.read();
            switch (self.cur) {
                std.math.maxInt(u21) => {
                    self.errorf("raw string literal not terminated", .{});
                    break;
                },
                '`' => {
                    _ = try std.unicode.utf8Encode(self.cur, &buf);
                    try self.tok_buffer.appendSlice(self.allocator, &buf);
                    self.read();
                    // can have an optional "i" ignore case suffix
                    if (self.cur == 'i') {
                        _ = try std.unicode.utf8Encode(self.cur, &buf);
                        try self.tok_buffer.appendSlice(self.allocator, &buf);
                        self.read();
                    }
                    break;
                },
                '\r' => {
                    has_cr = true;
                    // fallthrough
                },
                else => {
                    _ = try std.unicode.utf8Encode(self.cur, &buf);
                    try self.tok_buffer.appendSlice(self.allocator, &buf);
                },
            }
        }

        const result = try self.tok_buffer.toOwnedSlice(self.allocator);
        if (has_cr) {
            return try stripCR(self.allocator, result);
        }
        return result;
    }

    fn stripCR(allocator: std.mem.Allocator, bytes: []const u8) ![]const u8 {
        var result: std.ArrayList(u8) = .empty;
        for (bytes) |ch| {
            if (ch != '\r') {
                try result.append(allocator, ch);
            }
        }
        return result.toOwnedSlice(allocator);
    }

    fn scanString(self: *Scanner) ![]const u8 {
        self.tok_buffer.clearAndFree(self.allocator);
        var buf: [4]u8 = undefined;
        _ = try std.unicode.utf8Encode(self.cur, &buf);
        try self.tok_buffer.appendSlice(self.allocator, &buf); // opening '"'

        var noread = false;
        while (true) {
            if (!noread) {
                self.read();
            }
            noread = false;

            switch (self.cur) {
                '\\' => {
                    noread = !try self.scanEscape('"');
                },
                '\n', std.math.maxInt(u21) => {
                    // \n not consumed
                    self.errorf("string literal not terminated", .{});
                    return self.tok_buffer.toOwnedSlice(self.allocator);
                },
                '"' => {
                    _ = try std.unicode.utf8Encode(self.cur, &buf);
                    try self.tok_buffer.appendSlice(self.allocator, &buf);
                    self.read();
                    // can have an optional "i" ignore case suffix
                    if (self.cur == 'i') {
                        _ = try std.unicode.utf8Encode(self.cur, &buf);
                        try self.tok_buffer.appendSlice(self.allocator, &buf);
                        self.read();
                    }
                    return self.tok_buffer.toOwnedSlice(self.allocator);
                },
                else => {
                    _ = try std.unicode.utf8Encode(self.cur, &buf);
                    try self.tok_buffer.appendSlice(self.allocator, &buf);
                },
            }
        }
    }

    fn scanChar(self: *Scanner) ![]const u8 {
        self.tok_buffer.clearAndFree(self.allocator);
        var buf: [4]u8 = undefined;
        _ = try std.unicode.utf8Encode(self.cur, &buf);
        try self.tok_buffer.appendSlice(self.allocator, &buf); // opening "'"

        // must be followed by one char (which may be an escape) and a single
        // quote, but read until we find that closing quote.
        var cnt: usize = 0;
        var noread = false;
        while (true) {
            if (!noread) {
                self.read();
            }
            noread = false;

            switch (self.cur) {
                '\\' => {
                    cnt += 1;
                    noread = !try self.scanEscape('\'');
                },
                '\n', std.math.maxInt(u21) => {
                    // \n not consumed
                    self.errorf("rune literal not terminated", .{});
                    return self.tok_buffer.toOwnedSlice(self.allocator);
                },
                '\'' => {
                    _ = try std.unicode.utf8Encode(self.cur, &buf);
                    try self.tok_buffer.appendSlice(self.allocator, &buf);
                    self.read();
                    if (cnt != 1) {
                        self.errorf("rune literal is not a single rune", .{});
                    }
                    // can have an optional "i" ignore case suffix
                    if (self.cur == 'i') {
                        _ = try std.unicode.utf8Encode(self.cur, &buf);
                        try self.tok_buffer.appendSlice(self.allocator, &buf);
                        self.read();
                    }
                    return self.tok_buffer.toOwnedSlice(self.allocator);
                },
                else => {
                    cnt += 1;
                    _ = try std.unicode.utf8Encode(self.cur, &buf);
                    try self.tok_buffer.appendSlice(self.allocator, &buf);
                },
            }
        }
    }

    fn scanRuleDef(self: *Scanner) ![]const u8 {
        self.tok_buffer.clearAndFree(self.allocator);
        var buf: [4]u8 = undefined;
        _ = try std.unicode.utf8Encode(self.cur, &buf);
        try self.tok_buffer.appendSlice(self.allocator, &buf);
        const r = self.cur;
        self.read();
        if (r == '<') {
            if (self.cur != std.math.maxInt(u21)) {
                _ = try std.unicode.utf8Encode(self.cur, &buf);
                try self.tok_buffer.appendSlice(self.allocator, &buf);
            }
            if (self.cur != '-') {
                self.errorf("rule definition not terminated", .{});
            }
            self.read();
        }
        return self.tok_buffer.toOwnedSlice(self.allocator);
    }

    /// read advances Scanner to next rune.
    fn read(self: *Scanner) void {
        if (self.eof) {
            return;
        }

        // Read one byte at a time for simplicity
        const byte = self.reader.takeByte() catch |err| {
            self.fatalError(err);
            return;
        };

        // Store the byte as a Unicode code point
        self.cur = byte;
        self.cpos.offset += @intCast(self.cw);
        self.cw = 1;

        // newline is '\n' as in Go
        if (self.cur == '\n') {
            self.cpos.line += 1;
            self.cpos.column = 1;
        } else {
            self.cpos.column += 1;
        }
    }

    /// whitespace is same as Go, except that it doesn't skip newlines,
    /// those are returned as tokens.
    fn skipWhitespace(self: *Scanner) void {
        while (self.cur == ' ' or self.cur == '\t' or self.cur == '\r') {
            self.read();
        }
    }

    /// notify the handler of an error.
    fn errorp(self: *Scanner, pos: ast.Pos, err: anyerror) void {
        var need_free = false;
        const err_msg = blk: {
            const msg = std.fmt.allocPrint(self.allocator, "{any}", .{err}) catch {
                // 提供一个后备值给 err_msg
                break :blk "Unknown error";
            };
            need_free = true;
            break :blk msg;
        };

        defer if (need_free) self.allocator.free(err_msg);

        // if (self.errh) |errh| {
        //     errh(pos, err_msg);
        // } else {
            std.log.err("{f}: {s}", .{ pos, err_msg });
        // }
    }

    /// helper to generate and notify of an error.
    fn errorf(self: *Scanner, comptime fmt: []const u8, args: anytype) void {
        self.errorpf(self.cpos, fmt, args);
    }

    /// helper to generate and notify of an error at a specific position.
    fn errorpf(self: *Scanner, pos: ast.Pos, comptime fmt: []const u8, args: anytype) void {
        const err_msg = std.fmt.allocPrint(self.allocator, fmt, args) catch "Scanner error";
        defer self.allocator.free(err_msg);

        if (self.errh) |errh| {
            errh(pos, err_msg);
        } else {
            std.log.err("{f}: {s}", .{ pos, err_msg });
        }
    }

    /// notify a non-recoverable error that terminates scanning.
    fn fatalError(self: *Scanner, err: anyerror) void {
        self.cur = std.math.maxInt(u21);
        self.eof = true;
        if (err != ScannerError.EndOfStream) {
            self.errorp(self.cpos, err);
        }
    }

    pub fn deinit(self: *Scanner) void {
        self.tok_buffer.deinit(self.allocator);
        self.blacklisted.deinit();
    }
};

fn isRuleDefStart(r: u21) bool {
    return r == '=' or r == '<' or r == 0x2190 or r == 0x27f5; // Unicode arrows
}

/// isLetter has the same definition as Go.
fn isLetter(r: u21) bool {
    return ('a' <= r and r <= 'z') or
        ('A' <= r and r <= 'Z') or
        r == '_' or
        (r >= 0x80 and std.unicode.utf8ValidCodepoint(r));
}

/// isDigit has the same definition as Go.
fn isDigit(r: u21) bool {
    return ('0' <= r and r <= '9') or
        (r >= 0x80 and std.unicode.utf8ValidCodepoint(r));
}

fn digitVal(r: u21) u8 {
    if ('0' <= r and r <= '9') {
        return @intCast(r - '0');
    }
    if ('a' <= r and r <= 'f') {
        return @intCast(r - 'a' + 10);
    }
    if ('A' <= r and r <= 'F') {
        return @intCast(r - 'A' + 10);
    }
    return 16;
}
