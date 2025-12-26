const std = @import("std");

test "Check File.reader return type" {
    // Create a dummy file to check reader type
    const file = std.fs.File.stdout;
    
    var buffer: [1024]u8 = undefined;
    var file_reader = file.reader(&buffer);
    
    // Check what type file_reader is
    const info = @typeInfo(@TypeOf(file_reader));
    _ = info;
    _ = &file_reader.interface;
}
