const std = @import("std");
const common_lexer = @import("../common/lexer.zig");

// EBNF-specific lexer implementation
pub const EbnfLexer = struct {
    const Self = @This();

    input: []const u8,
    position: usize,
    line: usize,
    column: usize,
    allocator: std.mem.Allocator,
    peeked_token: ?common_lexer.Token = null,

    pub fn init(input: []const u8, allocator: std.mem.Allocator) Self {
        return Self{
            .input = input,
            .position = 0,
            .line = 1,
            .column = 1,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.* = undefined;
    }

    pub fn createLexer(allocator: std.mem.Allocator, input: []const u8) !common_lexer.Lexer {
        const lexer_ctx = try allocator.create(Self);
        lexer_ctx.* = init(input, allocator);

        const vtable_ptr = try allocator.create(common_lexer.Lexer.VTable);
        vtable_ptr.* = .{
            .nextToken = struct {
                fn func(ctx: *anyopaque) !common_lexer.Token {
                    const self = @as(*Self, @ptrCast(@alignCast(ctx)));
                    return self.nextToken();
                }
            }.func,
            .peekToken = struct {
                fn func(ctx: *anyopaque) !common_lexer.Token {
                    const self = @as(*Self, @ptrCast(@alignCast(ctx)));
                    return self.peekToken();
                }
            }.func,
            .deinit = struct {
                fn func(ctx: *anyopaque) void {
                    const self = @as(*Self, @ptrCast(@alignCast(ctx)));
                    self.deinit();
                }
            }.func,
        };

        return common_lexer.Lexer{
            .vtable = vtable_ptr,
            .ctx = lexer_ctx,
        };
    }

    pub fn nextToken(self: *Self) !common_lexer.Token {
        if (self.peeked_token) |token| {
            self.peeked_token = null;
            return token;
        }

        self.skipWhitespace();

        if (self.position >= self.input.len) {
            return common_lexer.Token{
                .type = .eof,
                .value = "",
                .line = self.line,
                .column = self.column,
            };
        }

        const current_char = self.input[self.position];
        const start_line = self.line;
        const start_column = self.column;

        switch (current_char) {
            '\n' => {
                self.position += 1;
                self.line += 1;
                self.column = 1;
                return common_lexer.Token{
                    .type = .eol,
                    .value = "\n",
                    .line = start_line,
                    .column = start_column,
                };
            },

            ':' => {
                self.position += 1;
                self.column += 1;
                return common_lexer.Token{
                    .type = .colon,
                    .value = ":",
                    .line = start_line,
                    .column = start_column,
                };
            },

            '.' => {
                self.position += 1;
                self.column += 1;
                return common_lexer.Token{
                    .type = .dot,
                    .value = ".",
                    .line = start_line,
                    .column = start_column,
                };
            },

            ';' => {
                self.position += 1;
                self.column += 1;
                return common_lexer.Token{
                    .type = .semicolon,
                    .value = ";",
                    .line = start_line,
                    .column = start_column,
                };
            },

            '(' => {
                self.position += 1;
                self.column += 1;
                return common_lexer.Token{
                    .type = .lparen,
                    .value = "(",
                    .line = start_line,
                    .column = start_column,
                };
            },

            ')' => {
                self.position += 1;
                self.column += 1;
                return common_lexer.Token{
                    .type = .rparen,
                    .value = ")",
                    .line = start_line,
                    .column = start_column,
                };
            },

            '{' => {
                self.position += 1;
                self.column += 1;
                const content = try self.scanCodeBlock();
                return common_lexer.Token{
                    .type = .code,
                    .value = content,
                    .line = start_line,
                    .column = start_column,
                };
            },

            '[' => {
                const content = try self.scanCharClass();
                return common_lexer.Token{
                    .type = .class,
                    .value = content,
                    .line = start_line,
                    .column = start_column,
                };
            },

            '&' => {
                self.position += 1;
                self.column += 1;
                return common_lexer.Token{
                    .type = .ampersand,
                    .value = "&",
                    .line = start_line,
                    .column = start_column,
                };
            },

            '!' => {
                self.position += 1;
                self.column += 1;
                return common_lexer.Token{
                    .type = .exclamation,
                    .value = "!",
                    .line = start_line,
                    .column = start_column,
                };
            },

            '?' => {
                self.position += 1;
                self.column += 1;
                return common_lexer.Token{
                    .type = .question,
                    .value = "?",
                    .line = start_line,
                    .column = start_column,
                };
            },

            '*' => {
                self.position += 1;
                self.column += 1;
                return common_lexer.Token{
                    .type = .star,
                    .value = "*",
                    .line = start_line,
                    .column = start_column,
                };
            },

            '+' => {
                self.position += 1;
                self.column += 1;
                return common_lexer.Token{
                    .type = .plus,
                    .value = "+",
                    .line = start_line,
                    .column = start_column,
                };
            },

            '/' => {
                self.position += 1;
                self.column += 1;
                return common_lexer.Token{
                    .type = .slash,
                    .value = "/",
                    .line = start_line,
                    .column = start_column,
                };
            },

            '"' => {
                const content = try self.scanString(false);
                return common_lexer.Token{
                    .type = .str,
                    .value = content,
                    .line = start_line,
                    .column = start_column,
                };
            },

            'r' => {
                if (self.position + 1 < self.input.len and self.input[self.position + 1] == '"') {
                    self.position += 2;
                    self.column += 2;
                    const content = try self.scanString(true);
                    return common_lexer.Token{
                        .type = .rstr,
                        .value = content,
                        .line = start_line,
                        .column = start_column,
                    };
                } else {
                    const ident = try self.scanIdentifier();
                    return common_lexer.Token{
                        .type = .ident,
                        .value = ident,
                        .line = start_line,
                        .column = start_column,
                    };
                }
            },

            '\'' => {
                const content = try self.scanChar();
                return common_lexer.Token{
                    .type = .char,
                    .value = content,
                    .line = start_line,
                    .column = start_column,
                };
            },

            else => {
                if (std.ascii.isAlphabetic(current_char)) {
                    const ident = try self.scanIdentifier();
                    return common_lexer.Token{
                        .type = .ident,
                        .value = ident,
                        .line = start_line,
                        .column = start_column,
                    };
                } else {
                    self.position += 1;
                    self.column += 1;
                    return common_lexer.Token{
                        .type = .invalid,
                        .value = self.input[start_line..start_line + 1],
                        .line = start_line,
                        .column = start_column,
                    };
                }
            },
        }
    }

    pub fn peekToken(self: *Self) !common_lexer.Token {
        if (self.peeked_token == null) {
            self.peeked_token = try self.nextToken();
        }
        return self.peeked_token.?;
    }

    fn skipWhitespace(self: *Self) void {
        while (self.position < self.input.len) {
            const c = self.input[self.position];
            switch (c) {
                ' ', '\t', '\r' => {
                    self.position += 1;
                    self.column += 1;
                },
                else => break,
            }
        }
    }

    fn scanIdentifier(self: *Self) ![]const u8 {
        const start = self.position;
        while (self.position < self.input.len) {
            const c = self.input[self.position];
            if (std.ascii.isAlphabetic(c) or std.ascii.isDigit(c) or c == '_') {
                self.position += 1;
                self.column += 1;
            } else {
                break;
            }
        }
        return self.input[start..self.position];
    }

    fn scanString(self: *Self, is_raw: bool) ![]const u8 {
        const start = self.position;
        while (self.position < self.input.len) {
            const c = self.input[self.position];
            self.position += 1;
            self.column += 1;

            if (c == '\\') {
                // Skip escaped character
                if (self.position < self.input.len) {
                    self.position += 1;
                    self.column += 1;
                }
            } else if (c == '"') {
                break;
            } else if (c == '\n') {
                // Unterminated string
                return error.UnterminatedString;
            }
        }

        if (self.position >= self.input.len) {
            return error.UnexpectedEOF;
        }

        // Return string without quotes (including r prefix for raw strings)
        if (is_raw) {
            return self.input[start - 2..self.position]; // Include r" prefix
        } else {
            return self.input[start..self.position];
        }
    }

    fn scanChar(self: *Self) ![]const u8 {
        self.position += 1; // Skip opening quote
        self.column += 1;
        const start = self.position;

        if (self.position >= self.input.len) {
            return error.UnexpectedEOF;
        }

        const c = self.input[self.position];
        self.position += 1;
        self.column += 1;

        if (c == '\\') {
            // Handle escaped character
            if (self.position < self.input.len) {
                self.position += 1;
                self.column += 1;
            }
        }

        if (self.position >= self.input.len or self.input[self.position] != '\'') {
            return error.UnterminatedChar;
        }

        self.position += 1; // Skip closing quote
        self.column += 1;

        return self.input[start..self.position - 1];
    }

    fn scanCharClass(self: *Self) ![]const u8 {
        self.position += 1; // Skip opening bracket
        self.column += 1;
        const start = self.position;

        while (self.position < self.input.len) {
            const c = self.input[self.position];
            self.position += 1;
            self.column += 1;

            if (c == ']') {
                break;
            } else if (c == '\n') {
                return error.UnterminatedCharClass;
            }
        }

        if (self.position >= self.input.len) {
            return error.UnexpectedEOF;
        }

        return self.input[start..self.position - 1];
    }

    fn scanCodeBlock(self: *Self) ![]const u8 {
        self.position += 1; // Skip opening brace
        self.column += 1;
        const start = self.position;
        var brace_count: usize = 1;

        while (self.position < self.input.len and brace_count > 0) {
            const c = self.input[self.position];
            self.position += 1;

            if (c == '\n') {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }

            if (c == '{') {
                brace_count += 1;
            } else if (c == '}') {
                brace_count -= 1;
            }
        }

        if (brace_count > 0) {
            return error.UnterminatedCodeBlock;
        }

        return self.input[start..self.position - 1];
    }
};