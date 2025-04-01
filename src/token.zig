const std = @import("std");

pub const TokenIndex = u32;

pub const Token = struct {
    pub const Tag = enum {
        invalid,
        lp,
        rp,
        quote,
        int,
        float,
        sym,
        str,
        eof,
    };

    pub const Loc = struct {
        start: usize,
        end: usize,
    };
    tag: Tag,
    loc: Loc,
};

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,

    const State = enum {
        invalid,
        start,
        sym,
        int,
        float,
        str,
        str_escape,
    };

    pub fn init(buffer: [:0]const u8) Tokenizer {
        // Skip the UTF-8 BOM if present.
        return .{
            .buffer = buffer,
            .index = 0,
        };
    }

    pub fn next(self: *Tokenizer) Token {
        var result: Token = .{
            .tag = undefined,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };

        state: switch (State.start) {
            // TODO fix this :)
            .invalid => unreachable,
            .start => switch (self.buffer[self.index]) {
                0 => {
                    if (self.index == self.buffer.len) {
                        return .{
                            .tag = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    } else {
                        continue :state .invalid;
                    }
                },
                '(' => {
                    result.tag = .lp;
                    self.index += 1;
                },
                ')' => {
                    result.tag = .rp;
                    self.index += 1;
                },
                ' ', '\n', '\t', '\r' => {
                    self.index += 1;
                    result.loc.start = self.index;
                    continue :state .start;
                },
                'a'...'z', 'A'...'Z', '_', '+', '-', '*', '/', '!', '<'...'>' => {
                    result.tag = .sym;
                    continue :state .sym;
                },
                '"' => {
                    result.tag = .str;
                    continue :state .str;
                },
                '\'' => {
                    result.tag = .quote;
                    self.index += 1;
                    continue :state .start;
                },
                // TODO add support for negative number
                '0'...'9' => {
                    result.tag = .int;
                    continue :state .int;
                },
                else => continue :state .invalid,
            },

            .str => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else {
                            result.tag = .invalid;
                        }
                    },
                    '\n' => result.tag = .invalid,
                    '\\' => continue :state .str_escape,
                    '"' => self.index += 1,
                    0x01...0x09, 0x0b...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    else => continue :state .str,
                }
            },

            .str_escape => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0, '\n' => result.tag = .invalid,
                    else => continue :state .str,
                }
            },

            .sym => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9', '+', '-', '*', '/', '!', '<'...'>' => continue :state .sym,
                    else => {},
                }
            },

            .int => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '0'...'9' => continue :state .int,
                    '.' => continue :state .float,
                    else => {},
                }
            },

            .float => {
                self.index += 1;
                if (result.tag == .float) @panic("lex fail, found 2 . in float");
                result.tag = .float;
                continue :state .int;
            },
        }

        result.loc.end = self.index;
        return result;
    }
};
