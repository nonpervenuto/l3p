const std = @import("std");

pub const ArgType = enum { variable, integerLiteral, dataLiteral, deref };
pub const Arg = union(ArgType) { variable: usize, integerLiteral: i32, dataLiteral: usize, deref: usize };

pub const DataType = enum { numeric };
pub const DeclarationType = enum { var_dec, global_dec };
pub const Declaration = union(DeclarationType) {
    var_dec: struct {
        name: []const u8,
        offset: usize,
        type: DataType,
    },
    global_dec: struct {
        name: []const u8,
        data: ?[]const u8,
        address: usize,
    },
};
pub const VariableList = std.ArrayList(Declaration);

// expressions
pub const OpType = enum {
    label,
    jump,
    jump_if_false,
    assign,
    index,
    call,
    prefix_neg,
    infix_or,
    infix_and,
    infix_bit_or,
    infix_bit_xor,
    infix_bit_and,
    infix_eq,
    infix_not_eq,
    infix_l,
    infix_le,
    infix_ge,
    infix_g,
    infix_shift_left,
    infix_shift_right,
    infix_plus,
    infix_minus,
    infix_multiply,
    infix_divide,
    infix_modulo,
};
pub const Op = union(OpType) {
    label: []const u8,
    jump: []const u8,
    jump_if_false: struct { label: []const u8, arg: Arg },
    assign: struct { offset: usize, arg: Arg },
    index: struct { offset: usize, offsetOf: usize, arg: Arg },
    call: struct { name: []const u8, args: []const Arg },
    prefix_neg: struct { offset: usize, arg: Arg },
    infix_or: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_and: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_bit_or: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_bit_xor: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_bit_and: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_eq: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_not_eq: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_l: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_le: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_ge: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_g: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_shift_left: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_shift_right: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_plus: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_minus: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_multiply: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_divide: struct { offset: usize, lhs: Arg, rhs: Arg },
    infix_modulo: struct { offset: usize, lhs: Arg, rhs: Arg },
};

pub const OperationList = std.ArrayList(Op);

// Ir
variables: VariableList,
operations: OperationList,
jump_labels: usize = 0,

pub fn createLabel(self: *@This(), allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
    const label = try std.fmt.allocPrint(allocator, "{s}_{d}", .{ name, self.jump_labels });
    self.jump_labels += 1;
    return label;
}

pub fn createTempVar(self: *@This(), dataType: DataType) !usize {
    const address = self.calcVarOffset(dataType);
    try self.variables.append(
        Declaration{
            .var_dec = .{ .name = "", .offset = address, .type = dataType },
        },
    );
    return address;
}

pub fn findVariable(self: *@This(), name: []const u8) ?Declaration {
    for (self.variables.items) |variable| {
        switch (variable) {
            .var_dec => |var_dec| {
                if (std.ascii.eqlIgnoreCase(var_dec.name, name)) {
                    return variable;
                }
            },
            .global_dec => {},
        }
    }
    return null;
}

pub fn calcVarOffset(self: *@This(), dataType: DataType) usize {
    var sum: usize = getVariableSize(dataType);
    for (self.variables.items) |variable| {
        sum += switch (variable) {
            .var_dec => |value| getVariableSize(value.type),
            .global_dec => 0,
        };
    }
    return sum;
}

pub fn calcGlobalOffset(self: *@This()) usize {
    var sum: usize = 0;
    for (self.variables.items) |variable| {
        sum += switch (variable) {
            .var_dec => 0,
            .global_dec => 1,
        };
    }
    return sum;
}

pub fn getStackSize(self: *@This()) usize {
    var size: usize = 0;
    for (self.variables.items) |variable| {
        size += switch (variable) {
            .var_dec => |value| getVariableSize(value.type),
            .global_dec => 0,
        };
    }
    return size;
}

pub fn getVariableSize(value: DataType) usize {
    return switch (value) {
        .numeric => 8,
    };
}
