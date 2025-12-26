//! Unit tests for SCC (Strongly Connected Components) analysis
//! Task 6.1: SCC 测试

const std = @import("std");
const scc_mod = @import("pegz_common").scc;
const Graph = scc_mod.Graph;
const SCC = scc_mod.SCC;
const stronglyConnectedComponents = scc_mod.stronglyConnectedComponents;
const findCyclesInSCC = scc_mod.findCyclesInSCC;

const testing = std.testing;

test "SCC - simple cycle" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create graph: A -> B -> C -> A
    var graph = Graph.init(allocator);
    try graph.addVertex("A");
    try graph.addVertex("B");
    try graph.addVertex("C");
    try graph.addEdge("A", "B");
    try graph.addEdge("B", "C");
    try graph.addEdge("C", "A");

    const sccs = try stronglyConnectedComponents(allocator, graph.vertices, &graph.edges);

    try testing.expectEqual(@as(usize, 1), sccs.items.len);
    try testing.expectEqual(@as(usize, 3), sccs.items[0].count());
}

test "SCC - two separate cycles" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create graph: A -> B -> A, C -> D -> C
    var graph = Graph.init(allocator);
    try graph.addVertex("A");
    try graph.addVertex("B");
    try graph.addVertex("C");
    try graph.addVertex("D");
    try graph.addEdge("A", "B");
    try graph.addEdge("B", "A");
    try graph.addEdge("C", "D");
    try graph.addEdge("D", "C");

    const sccs = try stronglyConnectedComponents(allocator, graph.vertices, &graph.edges);

    try testing.expectEqual(@as(usize, 2), sccs.items.len);
}

test "SCC - DAG" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create graph: A -> B -> C (no cycles)
    var graph = Graph.init(allocator);
    try graph.addVertex("A");
    try graph.addVertex("B");
    try graph.addVertex("C");
    try graph.addEdge("A", "B");
    try graph.addEdge("B", "C");

    const sccs = try stronglyConnectedComponents(allocator, graph.vertices, &graph.edges);

    // Each node is its own SCC
    try testing.expectEqual(@as(usize, 3), sccs.items.len);
}

test "findCyclesInSCC - simple cycle" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create graph: A -> B -> C -> A
    var graph = Graph.init(allocator);
    try graph.addVertex("A");
    try graph.addVertex("B");
    try graph.addVertex("C");
    try graph.addEdge("A", "B");
    try graph.addEdge("B", "C");
    try graph.addEdge("C", "A");

    var scc = SCC.init(allocator);
    try scc.put("A", {});
    try scc.put("B", {});
    try scc.put("C", {});

    const cycles = try findCyclesInSCC(allocator, &graph.edges, &scc, "A");

    try testing.expectEqual(@as(usize, 1), cycles.items.len);
    try testing.expectEqual(@as(usize, 4), cycles.items[0].items.len); // A -> B -> C -> A
}

test "SCC - single node" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create graph: A (single node, no edges)
    var graph = Graph.init(allocator);
    try graph.addVertex("A");

    const sccs = try stronglyConnectedComponents(allocator, graph.vertices, &graph.edges);

    // Single node is its own SCC
    try testing.expectEqual(@as(usize, 1), sccs.items.len);
    try testing.expectEqual(@as(usize, 1), sccs.items[0].count());
}

test "SCC - self loop" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create graph: A -> A (self loop)
    var graph = Graph.init(allocator);
    try graph.addVertex("A");
    try graph.addEdge("A", "A");

    const sccs = try stronglyConnectedComponents(allocator, graph.vertices, &graph.edges);

    // Self loop creates an SCC with one node
    try testing.expectEqual(@as(usize, 1), sccs.items.len);
    try testing.expectEqual(@as(usize, 1), sccs.items[0].count());
}

test "SCC - complex graph" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create graph:
    // A -> B -> C -> D
    //      |         ^
    //      v         |
    //      E <-------+
    var graph = Graph.init(allocator);
    try graph.addVertex("A");
    try graph.addVertex("B");
    try graph.addVertex("C");
    try graph.addVertex("D");
    try graph.addVertex("E");
    try graph.addEdge("A", "B");
    try graph.addEdge("B", "C");
    try graph.addEdge("B", "E");
    try graph.addEdge("C", "D");
    try graph.addEdge("D", "E");

    const sccs = try stronglyConnectedComponents(allocator, graph.vertices, &graph.edges);

    // No cycles, each node is its own SCC
    try testing.expectEqual(@as(usize, 5), sccs.items.len);
}
