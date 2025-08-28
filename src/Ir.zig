const std = @import("std");

pub const ArgType = enum { variable, integerLiteral, dataLiteral, deref };
pub const Arg = union(ArgType) { variable: usize, integerLiteral: u32, dataLiteral: usize, deref: usize };

pub const DataType = enum { pointer, number, boolean, char };
pub const VarType = union(enum) { primitive: DataType, array: struct { len: usize, type: DataType } };

pub const GlobalVar = struct {
    name: []const u8,
    data: ?[]const u8,
    address: usize,
};

pub const ScopedVar = struct {
    name: []const u8,
    offset: usize,
    type: VarType,
};

pub const GlobalVarList = std.ArrayList(GlobalVar);
pub const ScopedVarList = std.ArrayList(ScopedVar);

// expressions
pub const OpType = enum {
    label,
    jump,
    jump_if_false,
    assign,
    ref,
    store,
    index,
    call,
    ret,
    unary_not,
    unary_neg,
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
    assign: struct { lhs: Arg, rhs: Arg },
    // take the address
    ref: struct { offset: usize, arg: Arg },
    // store the value at the given address
    store: struct { offset: usize, arg: Arg },
    index: struct { offset: usize, var_address: usize, var_index: Arg },
    call: struct { name: []const u8, offset: usize, args: []const Arg },
    ret: struct { arg: Arg },
    unary_not: struct { offset: usize, arg: Arg },
    unary_neg: struct { offset: usize, arg: Arg },
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
pub const FunctionList = std.ArrayList(Function);

pub const Body = struct {
    variables: ScopedVarList,
    operations: OperationList,
};

pub const Function = struct {
    name: []const u8,
    body: Body,
};

// Ir
globals: GlobalVarList,
body: Body,
functions: FunctionList,
jump_labels: usize = 0,

pub fn createLabel(self: *@This(), allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
    const label = try std.fmt.allocPrint(allocator, "{s}_{d}", .{ name, self.jump_labels });
    self.jump_labels += 1;
    return label;
}

pub fn createVar(self: *@This(), allocator: std.mem.Allocator, body: *Body, name: []const u8, dataType: VarType) !usize {
    const address = self.calcVarOffset(body, dataType);
    try body.variables.append(
        allocator,
        .{ .name = name, .offset = address, .type = dataType },
    );
    return address;
}

pub fn createTempVar(self: *@This(), allocator: std.mem.Allocator, body: *Body, dataType: VarType) !usize {
    const address = self.calcVarOffset(body, dataType);
    try body.variables.append(
        allocator,
        .{ .name = "", .offset = address, .type = dataType },
    );
    return address;
}

pub fn findVariable(self: *@This(), body: *Body, name: []const u8) ?ScopedVar {
    _ = self;
    for (body.variables.items) |variable| {
        if (std.ascii.eqlIgnoreCase(variable.name, name)) {
            return variable;
        }
    }
    return null;
}

pub fn calcVarOffset(self: *@This(), body: *Body, varType: VarType) usize {
    _ = self;
    var sum: usize = getVariableSize(varType);
    for (body.variables.items) |variable| {
        sum += getVariableSize(variable.type);
    }
    return sum;
}

pub fn calcGlobalOffset(self: *@This()) usize {
    return self.globals.items.len;
}

pub fn getStackSize(self: *@This(), body: *Body) usize {
    _ = self;
    var size: usize = 0;
    for (body.variables.items) |variable| {
        size += getVariableSize(variable.type);
    }
    const mod = @mod(size, 8);
    return if (mod > 0) size + (8 - mod) else size;
}

pub fn getVariableSize(varType: VarType) usize {
    return switch (varType) {
        .primitive => |primitive| {
            return switch (primitive) {
                .pointer => 8,
                .number => 8,
                .char => 1,
                .boolean => 1,
            };
        },
        .array => |array| 8 * array.len,
    };
}
