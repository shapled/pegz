//! Strongly Connected Components (SCC) analysis using Tarjan's algorithm
//! Based on pigeon's builder/scc.go

const std = @import("std");
const StringHashMap = std.StringHashMap;
const ArrayList = std.ArrayList;

const Self = @This();

/// Graph represents a directed graph using adjacency lists
pub const Graph = struct {
    allocator: std.mem.Allocator,
    vertices: ArrayList([]const u8),
    edges: StringHashMap(StringHashMap(void)),

    /// Initialize a new graph
    pub fn init(allocator: std.mem.Allocator) Graph {
        return .{
            .allocator = allocator,
            .vertices = ArrayList([]const u8).initCapacity(allocator, 0) catch unreachable,
            .edges = StringHashMap(StringHashMap(void)).init(allocator),
        };
    }

    /// Add a vertex to the graph
    pub fn addVertex(self: *Graph, vertex: []const u8) !void {
        try self.vertices.append(self.allocator, vertex);
        if (!self.edges.contains(vertex)) {
            try self.edges.put(vertex, StringHashMap(void).init(self.allocator));
        }
    }

    /// Add a directed edge from src to dst
    pub fn addEdge(self: *Graph, src: []const u8, dst: []const u8) !void {
        const dst_map = try self.edges.getOrPut(src);
        if (!dst_map.found_existing) {
            dst_map.value_ptr.* = StringHashMap(void).init(self.allocator);
        }
        try dst_map.value_ptr.put(dst, {});
    }

    /// Check if an edge exists
    pub fn hasEdge(self: *const Graph, src: []const u8, dst: []const u8) bool {
        if (self.edges.get(src)) |dsts| {
            return dsts.contains(dst);
        }
        return false;
    }

    /// Get all outgoing edges from a vertex
    pub fn getOutgoing(self: *const Graph, vertex: []const u8) ?*const StringHashMap(void) {
        return self.edges.get(vertex);
    }
};

/// SCC represents a strongly connected component
pub const SCC = StringHashMap(void);

/// Compute strongly connected components using Tarjan's algorithm
/// Returns an ArrayList of SCCs
pub fn stronglyConnectedComponents(
    allocator: std.mem.Allocator,
    vertices: ArrayList([]const u8),
    edges: *const StringHashMap(StringHashMap(void)),
) !ArrayList(SCC) {
    var result = try ArrayList(SCC).initCapacity(allocator, 0);

    // Tarjan's algorithm state
    var index = std.StringHashMap(usize).init(allocator);
    var lowlink = std.StringHashMap(usize).init(allocator);
    var stack = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    var on_stack = std.StringHashMap(bool).init(allocator);
    var identified = std.StringHashMap(bool).init(allocator);
    var next_index: usize = 0;

    // Process each vertex
    for (vertices.items) |vertex| {
        if (!index.contains(vertex)) {
            try dfsTarjan(
                allocator,
                vertex,
                edges,
                &index,
                &lowlink,
                &stack,
                &on_stack,
                &identified,
                &next_index,
                &result,
            );
        }
    }

    return result;
}

/// Helper function for Tarjan's DFS algorithm
fn dfsTarjan(
    allocator: std.mem.Allocator,
    vertex: []const u8,
    edges: *const StringHashMap(StringHashMap(void)),
    index: *std.StringHashMap(usize),
    lowlink: *std.StringHashMap(usize),
    stack: *std.ArrayList([]const u8),
    on_stack: *std.StringHashMap(bool),
    identified: *std.StringHashMap(bool),
    next_index: *usize,
    result: *std.ArrayList(SCC),
) !void {
    // Set the depth index for vertex to the smallest unused index
    try index.put(vertex, next_index.*);
    try lowlink.put(vertex, next_index.*);
    next_index.* += 1;
    try stack.append(allocator, vertex);
    try on_stack.put(vertex, true);

    // Consider successors of vertex
    if (edges.get(vertex)) |dsts| {
        var dst_iter = dsts.iterator();
        while (dst_iter.next()) |entry| {
            const w = entry.key_ptr.*;
            if (!index.contains(w)) {
                // Successor w has not yet been visited; recurse on it
                try dfsTarjan(
                    allocator,
                    w,
                    edges,
                    index,
                    lowlink,
                    stack,
                    on_stack,
                    identified,
                    next_index,
                    result,
                );
                if (lowlink.get(w)) |w_lowlink| {
                    if (lowlink.get(vertex)) |v_lowlink| {
                        if (w_lowlink < v_lowlink) {
                            lowlink.put(vertex, w_lowlink) catch {};
                        }
                    }
                }
            } else if (on_stack.get(w)) |w_on_stack| {
                if (w_on_stack) {
                    // Successor w is in stack S and hence in the current SCC
                    if (lowlink.get(w)) |w_lowlink| {
                        if (lowlink.get(vertex)) |v_lowlink| {
                            if (w_lowlink < v_lowlink) {
                                lowlink.put(vertex, w_lowlink) catch {};
                            }
                        }
                    }
                }
            }
        }
    }

    // If vertex is a root node, pop the stack and generate an SCC
    if (index.get(vertex)) |v_index| {
        if (lowlink.get(vertex)) |v_lowlink| {
            if (v_index == v_lowlink) {
                // Start a new strongly connected component
                var scc = SCC.init(allocator);
                var v_clone: []const u8 = undefined;

                while (true) {
                    v_clone = stack.pop() orelse unreachable;
                    try on_stack.put(v_clone, false);
                    try scc.put(v_clone, {});
                    try identified.put(v_clone, true);

                    if (std.mem.eql(u8, v_clone, vertex)) {
                        break;
                    }
                }
                try result.append(allocator, scc);
            }
        }
    }
}

/// Reduce the graph to only contain nodes in the given SCC
pub fn reduceGraph(
    allocator: std.mem.Allocator,
    edges: *const StringHashMap(StringHashMap(void)),
    scc: *const SCC,
) !StringHashMap(StringHashMap(void)) {
    var result = StringHashMap(StringHashMap(void)).init(allocator);

    var edge_iter = edges.iterator();
    while (edge_iter.next()) |entry| {
        const src = entry.key_ptr.*;
        const dsts = entry.value_ptr;

        // Skip if src not in SCC
        if (!scc.contains(src)) {
            continue;
        }

        // Create new destination map
        const new_dsts_entry = try result.getOrPut(src);
        if (!new_dsts_entry.found_existing) {
            new_dsts_entry.value_ptr.* = StringHashMap(void).init(allocator);
        }

        var dst_iter = dsts.iterator();
        while (dst_iter.next()) |dst_entry| {
            const dst = dst_entry.key_ptr.*;
            // Only include edges to nodes in the SCC
            if (scc.contains(dst)) {
                try new_dsts_entry.value_ptr.put(dst, {});
            }
        }
    }

    return result;
}

/// Find cycles in an SCC starting from the start node
/// Returns an ArrayList of cycles, where each cycle is a list of vertices
pub fn findCyclesInSCC(
    allocator: std.mem.Allocator,
    edges: *const StringHashMap(StringHashMap(void)),
    scc: *const SCC,
    start: []const u8,
) !ArrayList(ArrayList([]const u8)) {
    // Validate that start is in SCC
    if (!scc.contains(start)) {
        return error.StartNotInSCC;
    }

    // Validate that all SCC vertices have edges defined
    {
        var scc_iter = scc.iterator();
        while (scc_iter.next()) |entry| {
            const vertex = entry.key_ptr.*;
            if (!edges.contains(vertex)) {
                return error.VertexNotInGraph;
            }
        }
    }

    // Reduce the graph to nodes in the SCC
    const reduced_edges = try reduceGraph(allocator, edges, scc);

    // Validate that start is in reduced graph
    if (!reduced_edges.contains(start)) {
        return error.StartNotInReducedGraph;
    }

    var result = try ArrayList(ArrayList([]const u8)).initCapacity(allocator, 0);

    // DFS to find cycles
    var path = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    try findCyclesDFS(allocator, start, &reduced_edges, &path, &result);

    return result;
}

/// Helper DFS function to find cycles
fn findCyclesDFS(
    allocator: std.mem.Allocator,
    node: []const u8,
    edges: *const StringHashMap(StringHashMap(void)),
    path: *std.ArrayList([]const u8),
    result: *std.ArrayList(ArrayList([]const u8)),
) !void {
    // Check if node is already in path (cycle found)
    for (path.items) |v| {
        if (std.mem.eql(u8, v, node)) {
            // Found a cycle - create cycle list from node occurrence to end
            var cycle = try std.ArrayList([]const u8).initCapacity(allocator, 0);
            var found = false;
            for (path.items) |item| {
                if (std.mem.eql(u8, item, node)) {
                    found = true;
                }
                if (found) {
                    try cycle.append(allocator, item);
                }
            }
            try cycle.append(allocator, node);
            try result.append(allocator, cycle);
            return;
        }
    }

    // Add node to path and recurse
    try path.append(allocator, node);

    if (edges.get(node)) |dsts| {
        var dst_iter = dsts.iterator();
        while (dst_iter.next()) |entry| {
            const child = entry.key_ptr.*;
            try findCyclesDFS(allocator, child, edges, path, result);
        }
    }

    _ = path.pop();
}
