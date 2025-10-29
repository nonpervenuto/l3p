const std = @import("std");

const Lexer = @import("Lexer.zig");

pub fn main() !void {
    var lexer = Lexer.init("test", "3 * 4 + 1");
    var tree = ArrayBinaryTree.init(std.heap.page_allocator);

    _ = try parseExpression(&tree, &lexer, .root, null, 0);

    // const Foo = union(enum) {
    //     a: u32,
    //     b: []const u8,
    // };
    // var list = std.MultiArrayList(Foo){};
    // try list.append(std.heap.page_allocator, .{ .b = "asd" });
    // const a = list.items(.data)[0].b;
    // std.debug.print("{s}\n", .{a});

    try tree.print_dfs();

    // const slice = tree.tree.items;

    // var i: usize = 0;
    // while (i < slice.len) : (i += 1) {
    //     const node: Node = slice[i];
    //     std.debug.print("{d} {f}\n", .{ i, node });
    // }
}

fn parsePrimary(lexer: *Lexer) !?Node {
    const token = lexer.next() orelse return null;
    switch (token.kind) {
        .IntegerLiteral => {
            return .{ .value = try token.asInteger() };
        },
        else => {
            std.debug.print("Not implemented {t}\n", .{token.kind});
            @panic("Not implemented");
        },
    }
}

fn parseExpression(tree: *ArrayBinaryTree, lexer: *Lexer, nodeIndex: NodeIndex, optValue: ?Node, min_precedence: usize) !NodeIndex {
    var lhs = if (optValue) |value| value else try parsePrimary(lexer);
    var root = nodeIndex;

    while (lexer.peek()) |token| {
        const op: BinOp = switch (token.kind) {
            .Plus => BinOp.add,
            .Minus => BinOp.sub,
            .Multiply => BinOp.mul,
            .Divide => BinOp.div,
            else => @panic("Not implemented"),
        };
        const precedence: usize = switch (token.kind) {
            .Plus, .Minus => 0,
            .Multiply, .Divide => 1,
            else => 0,
        };

        if (precedence > min_precedence) {}

        _ = lexer.next();
        _ = try tree.set(root, .{ .binOp = op });
        _ = try tree.setRight(root, lhs.?);
        root = root.leftIndex();
        lhs = try parsePrimary(lexer);
    }

    // std.debug.print("Root: {d} -> {f}\n", .{ root, lhs.? });
    if (lhs != null) {
        _ = try tree.set(root, lhs.?);
    }
    return root;
}

const NodeIndex = enum(usize) {
    root = 0,
    _,

    fn leftIndex(self: NodeIndex) NodeIndex {
        return @enumFromInt(2 * self.toUsize() + 1);
    }

    fn rightIndex(self: NodeIndex) NodeIndex {
        return @enumFromInt(2 * self.toUsize() + 2);
    }

    fn toUsize(self: NodeIndex) usize {
        return @intFromEnum(self);
    }
};

const BinOp = enum { add, sub, mul, div };

const Node = union(enum) {
    root: void,
    none: void,
    value: u32,
    binOp: BinOp,

    pub fn format(self: Node, w: *std.Io.Writer) !void {
        switch (self) {
            .root => try w.print("ROOT", .{}),
            .none => try w.print("none", .{}),
            .value => |value| try w.print("{d}", .{value}),
            .binOp => |op| try w.print("{t}", .{op}),
        }
    }
};

const Element = struct {
    index: NodeIndex,
    node: Node,
};

const Order = enum {
    pre_order,
    in_order,
    post_order,
};

const Nodes = std.ArrayList(Node);
const ArrayBinaryTree = struct {
    gpa: std.mem.Allocator,
    tree: Nodes,

    pub fn init(gpa: std.mem.Allocator) ArrayBinaryTree {
        return .{
            .gpa = gpa,
            .tree = .empty,
        };
    }

    pub fn size(self: *ArrayBinaryTree) usize {
        return self.tree.items.len;
    }

    pub fn val(self: *ArrayBinaryTree, index: usize) Node {
        if (index < 0 or index >= self.size()) {
            return .none;
        }
        return self.tree.items[index];
    }

    fn left(index: usize) usize {
        return 2 * index + 1;
    }

    fn right(index: usize) usize {
        return 2 * index + 2;
    }

    fn parent(index: usize) usize {
        if (index == 0) return 0;
        return @divTrunc(index - 1, 2);
    }

    pub fn set(self: *ArrayBinaryTree, nodeIndex: NodeIndex, node: Node) !NodeIndex {
        try self.insert(@intFromEnum(nodeIndex), node);
        return nodeIndex;
    }

    pub fn setLeft(self: *ArrayBinaryTree, nodeIndex: NodeIndex, node: Node) !NodeIndex {
        const leftIndex = left(@intFromEnum(nodeIndex));
        try self.insert(leftIndex, node);
        return @enumFromInt(leftIndex);
    }

    pub fn setRight(self: *ArrayBinaryTree, nodeIndex: NodeIndex, node: Node) !NodeIndex {
        const rightIndex = right(@intFromEnum(nodeIndex));
        try self.insert(rightIndex, node);
        return @enumFromInt(rightIndex);
    }

    fn append(self: *ArrayBinaryTree, node: Node) !usize {
        try self.tree.append(self.gpa, node);
        return self.tree.items.len - 1;
    }

    fn insert(self: *ArrayBinaryTree, index: usize, node: Node) !void {
        if (self.size() == 0 or self.size() - 1 < index) {
            // std.debug.print("Appending {d} {f}\n", .{ index, node });
            const n = index - self.size();
            try self.tree.appendNTimes(self.gpa, .none, n);
            try self.tree.append(self.gpa, node);
        } else {
            // std.debug.print("Replace {d} {f}\n", .{ index, node });
            self.tree.replaceRangeAssumeCapacity(index, 1, &.{node});
        }
    }

    fn dfs(self: *ArrayBinaryTree, index: usize, order: Order, res: *Nodes) void {
        if (self.val(index) == .none) return;
        if (order == .pre_order) {
            res.append(self.gpa, self.val(index)) catch unreachable;
        }
        self.dfs(left(index), order, res);
        if (order == .in_order) {
            res.append(self.gpa, self.val(index)) catch unreachable;
        }
        self.dfs(right(index), order, res);
        if (order == .post_order) {
            res.append(self.gpa, self.val(index)) catch unreachable;
        }
    }

    pub fn print_dfs(self: *ArrayBinaryTree) !void {
        var res: std.ArrayList(u8) = .empty;

        if (self.val(0) == .none) {
            try res.appendSlice(self.gpa, "");
            return;
        }

        const str = try std.fmt.allocPrint(self.gpa, "{f}", .{self.val(0)});
        try res.appendSlice(self.gpa, str);

        const pointerRight = "└──";
        const pointerLeft = if (self.val(right(0)) != .none) "├──" else "└──";

        try self.print_nodes("", pointerLeft, left(0), self.val(right(0)) != .none, &res);
        try self.print_nodes("", pointerRight, right(0), false, &res);

        std.debug.print("{s}\n", .{res.items});
    }

    fn print_nodes(self: *ArrayBinaryTree, padding: []const u8, pointer: []const u8, index: usize, hasRight: bool, res: *std.ArrayList(u8)) !void {
        if (self.val(index) != .none) {
            try res.appendSlice(self.gpa, "\n");
            try res.appendSlice(self.gpa, padding);
            try res.appendSlice(self.gpa, pointer);

            const str = try std.fmt.allocPrint(self.gpa, "{f}", .{self.val(index)});
            try res.appendSlice(self.gpa, str);

            const paddingForBoth = try std.fmt.allocPrint(self.gpa, "{s}{s}", .{ padding, if (hasRight) "|   " else "    " });

            const pointerRight = "└──";
            const pointerLeft = if (self.val(right(index)) != .none) "├──" else "└──";

            try self.print_nodes(paddingForBoth, pointerLeft, left(index), self.val(right(index)) != .none, res);
            try self.print_nodes(paddingForBoth, pointerRight, right(index), false, res);
        }
    }

    pub fn preOrder(self: *ArrayBinaryTree) []Node {
        var res: Nodes = .empty;
        self.dfs(0, Order.pre_order, &res);
        return res.toOwnedSlice(self.gpa) catch unreachable;
    }
    pub fn inOrder(self: *ArrayBinaryTree) []Node {
        var res: Nodes = .empty;
        self.dfs(0, Order.in_order, &res);
        return res.toOwnedSlice(self.gpa) catch unreachable;
    }
    pub fn postOrder(self: *ArrayBinaryTree) Nodes.Slice {
        var res: Nodes = .empty;
        self.dfs(0, Order.post_order, &res);
        return res.toOwnedSlice(self.gpa) catch unreachable;
    }
};

//
//     def in_order(self) -> list[int]:
//         """In-order traversal"""
//         self.res = []
//         self.dfs(0, order="in")
//         return self.res
//
//     def post_order(self) -> list[int]:
//         """Post-order traversal"""
//         self.res = []
//         self.dfs(0, order="post")
//         return self.res
//
