-module( mesh ).
-export( [tessellate_sphere/2,
          output_mesh/2,
          mesh_to_graph/1,
          add_distinct_edge/3,
          edge_exists/3] ).

% The mesh record represents a sphere during the generation phase. 
% The mesh is represented by a number of vertices on the surface of a sphere.
%
% The first element, number_of_vertices, is the number of vertices in the mesh. 
% This number should agree with the length of the fourth element.
%
% The second element, number_of_faces, is the number of faces in the mesh.
% This number should agree with the length of the fifth element.
%
% The third element, number_of_edges, is the number of edges connecting the vertices.
% The edges themselves are not explicitely stored but can be computed from the faces.
%
% The fourth element, vertices, is the list containing all vertices.
% Each vertex stored in the list as a tuple wth two elements, first one is the 
% vertex identifier, and the second one is a tuple containing the three cartesian 
% coordinates for the vertex.
% 
% The fifth element, faces, is the list containing all faces.
% Each face stored as a tuple with two elements, first one is the face identifier,
% second one is a tuple containing the three vertex identifiers of the vertices 
% which make up the face.
-record( mesh, {
    number_of_vertices,
    number_of_faces,
    number_of_edges,
    vertices,
    faces } ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% tessellate_sphere/2 initializes a mesh with Initial_Polyhedron data,
% then subdivides it Subdivisions times, and then normalizes all vertices.

tessellate_sphere( Initial_Polyhedron, Subdivisions ) ->
    normalize( subdivide( Subdivisions, initialize_mesh( Initial_Polyhedron ) ) ).

%--------------------------------------------------------------------------------------------------%
% initialize_mesh/1
%
% Returns a mesh record initialized with the vertices and faces of the specified polyhedron.
% The first arguemnt can be 
%   tetrahedron
%   octahedron
%   icosahedron
%
initialize_mesh( tetrahedron ) -> 
    Sqrt3 = 1 / math:sqrt( 3.0 ),
    #mesh{
        number_of_vertices = 4,
        number_of_faces    = 4,
        number_of_edges    = 6,
        vertices =
            [{ {  Sqrt3,  Sqrt3,  Sqrt3 } },
             { { -Sqrt3, -Sqrt3,  Sqrt3 } },
             { { -Sqrt3,  Sqrt3, -Sqrt3 } },
             { {  Sqrt3, -Sqrt3, -Sqrt3 } }],
        faces =
            [{ 3, { 0, 2, 1 } }, 
             { 2, { 0, 1, 3 } }, 
             { 1, { 2, 3, 1 } }, 
             { 0, { 3, 2, 0 } }]
    };
initialize_mesh( octahedron  ) ->
    #mesh{
        number_of_vertices = 6,
        number_of_faces    = 8,
        number_of_edges    = 12,
        vertices = 
            [{ 5, {  0.0,  0.0, -1.0 } },
             { 4, {  1.0,  0.0,  0.0 } },
             { 3, {  0.0, -1.0,  0.0 } },
             { 2, { -1.0,  0.0,  0.0 } },
             { 1, {  0.0,  1.0,  0.0 } },
             { 0, {  0.0,  0.0,  1.0 } }],
        faces =
            [{ 7, { 0, 1, 2 } }, 
             { 6, { 0, 2, 3 } }, 
             { 5, { 0, 3, 4 } }, 
             { 4, { 0, 4, 1 } }, 
             { 3, { 5, 2, 1 } }, 
             { 2, { 5, 3, 2 } }, 
             { 1, { 5, 4, 3 } },
             { 0, { 5, 1, 4 } }]
    };
initialize_mesh( icosahedron ) -> 
    Phi = (1 + math:sqrt(5)) / 2,
    Tau = Phi / math:sqrt( 1 + Phi * Phi ),
    One = 1   / math:sqrt( 1 + Phi * Phi ),
    #mesh{
        number_of_vertices = 12,
        number_of_faces    = 20,
        number_of_edges    = 30,
        vertices = 
            [{ 11, {  0.0,  Tau, -One } },
             { 10, {  0.0, -Tau, -One } },
             {  9, {  0.0, -Tau,  One } },
             {  8, {  0.0,  Tau,  One } },
             {  7, { -One,  0.0,  Tau } },
             {  6, { -One,  0.0, -Tau } },
             {  5, {  One,  0.0, -Tau } },
             {  4, {  One,  0.0,  Tau } },
             {  3, {  Tau, -One,  0.0 } },
             {  2, { -Tau, -One,  0.0 } },
             {  1, { -Tau,  One,  0.0 } },
             {  0, {  Tau,  One,  0.0 } }],
        faces =
            [{ 19, {  6, 10,  2 } },
             { 18, {  7,  2,  9 } },
             { 17, {  6,  1, 11 } },
             { 16, {  7,  8,  1 } },
             { 15, {  5,  3, 10 } },
             { 14, {  4,  9,  3 } },
             { 13, { 11,  0,  5 } },
             { 12, {  8,  4,  0 } },
             { 11, {  9,  2, 10 } },
             { 10, {  9, 10,  3 } },
             {  9, {  8, 11,  1 } },
             {  8, {  8,  0, 11 } },
             {  7, {  2,  1,  6 } },
             {  6, {  2,  7,  1 } },
             {  5, {  0,  3,  5 } },
             {  4, {  0,  4,  3 } },
             {  3, {  5, 10,  6 } },
             {  2, {  5,  6, 11 } },
             {  1, {  4,  7,  9 } },
             {  0, {  4,  8,  7 } }]
    }.

%--------------------------------------------------------------------------------------------------%

subdivide( 0, Mesh ) -> Mesh;
subdivide( N, Mesh ) ->
    subdivide( N - 1, subdivide_once( Mesh) ).

%--------------------------------------------------------------------------------------------------%

subdivide_once( Mesh ) ->
    Old_Number_of_Vertices  = Mesh#mesh.number_of_vertices,
    Old_Number_of_Faces     = Mesh#mesh.number_of_faces,
    Old_Vertices            = Mesh#mesh.vertices,
    Old_Faces               = Mesh#mesh.faces,
    New_Number_of_Edges     = 2 * Old_Number_of_Vertices + 3 * Old_Number_of_Faces,

    { New_Number_of_Vertices, New_Number_of_Faces, New_Vertices, New_Faces } = 
        subdivide_face( Old_Faces, Old_Number_of_Vertices, 0, Old_Vertices, [], [] ),
    
    #mesh{
        number_of_vertices  = New_Number_of_Vertices,
        number_of_faces     = New_Number_of_Faces,
        number_of_edges     = New_Number_of_Edges,
        vertices            = New_Vertices,
        faces               = New_Faces
    }.

%--------------------------------------------------------------------------------------------------%

subdivide_face( [], 
                New_Number_of_Vertices, 
                New_Number_of_Faces, 
                New_Vertices, 
                New_Faces, 
                _ ) -> 
    { New_Number_of_Vertices, New_Number_of_Faces, New_Vertices, New_Faces };
subdivide_face( [Face|Faces], 
                Number_of_Vertices, 
                Number_of_Faces, 
                Vertices, 
                New_Faces, 
                Midpoints ) ->
    { _, { A, B, C } } = Face,

    % The number of vertices, the list of vertices, and the list of midpoints are all passed to 
    % midpoint/5 and returned in a tuple because they may be modified. 
    { AB_midpoint, Number_of_Vertices1, Vertices1, Midpoints1 } = 
        midpoint( A, B, Number_of_Vertices,  Vertices,  Midpoints  ),

    { BC_midpoint, Number_of_Vertices2, Vertices2, Midpoints2 } = 
        midpoint( B, C, Number_of_Vertices1, Vertices1, Midpoints1 ),

    { CA_midpoint, Number_of_Vertices3, Vertices3, Midpoints3 } = 
        midpoint( C, A, Number_of_Vertices2, Vertices2, Midpoints2 ),

    Face1 = { Number_of_Faces + 1, { A,           AB_midpoint, CA_midpoint } },
    Face2 = { Number_of_Faces + 2, { CA_midpoint, AB_midpoint, BC_midpoint } },
    Face3 = { Number_of_Faces + 3, { CA_midpoint, BC_midpoint, C           } },
    Face4 = { Number_of_Faces + 4, { AB_midpoint, B,           BC_midpoint } },
    
    subdivide_face( Faces, 
                    Number_of_Vertices3, 
                    Number_of_Faces + 4, 
                    Vertices3, 
                    [Face4, Face3, Face2, Face1 | New_Faces], 
                    Midpoints3 ).

%--------------------------------------------------------------------------------------------------%

midpoint( Start, End, Number_of_Vertices, Vertices, Midpoints ) ->
    Exists = exists( Start, End, Midpoints ),
    case Exists of
        false ->
            % Vertex not in list, so add and create it.
            { Start, { X1, X2, X3 } } = get_mesh_vertex( Vertices, Start ),
            { End,   { Y1, Y2, Y3 } } = get_mesh_vertex( Vertices, End   ),
            Z1 = (X1 + Y1) / 2.0,
            Z2 = (X2 + Y2) / 2.0,
            Z3 = (X3 + Y3) / 2.0,
            {
                % Number_of_Vertices is the index for the newly created vertex.
                Number_of_Vertices,                                  % Return midpoint vertex index.
                Number_of_Vertices + 1,                              % Return incremented number of vertices.
                [{ Number_of_Vertices, { Z1, Z2, Z3 } } | Vertices], % Return vertices.
                [{ Start, Number_of_Vertices, End }     | Midpoints] % Return faces.
            };
        Midpoint ->
            % Vertex already exists, return it.
            {
                Midpoint,
                Number_of_Vertices,
                Vertices, 
                Midpoints
            }
    end.

%--------------------------------------------------------------------------------------------------%

exists( _, _, [] ) -> 
    false;
exists( Start, End, [ { S, M, E } | _] ) 
    when (  ( ( S == Start ) and ( E == End ) ) 
         or ( ( S == End ) and ( E == Start ) ) ) -> 
    M;
exists( Start, End, [ _ | Midpoints] ) -> 
    exists( Start, End, Midpoints ).

%--------------------------------------------------------------------------------------------------%

get_mesh_vertex( [],                           _     ) -> 
    error;
get_mesh_vertex( [{ Index, Coordinates } | _], Index ) -> 
    { Index, Coordinates };
get_mesh_vertex( [ _ | Vertices]             , Index ) -> 
    get_mesh_vertex( Vertices, Index ).

%--------------------------------------------------------------------------------------------------%

normalize( Mesh ) ->
    Vertices = Mesh#mesh.vertices,
    Normalized_Vertices = normalize_vertices( Vertices, [] ),
    #mesh{
        number_of_vertices = Mesh#mesh.number_of_vertices,
        number_of_faces    = Mesh#mesh.number_of_faces,
        number_of_edges    = Mesh#mesh.number_of_edges,
        vertices           = Normalized_Vertices,
        faces              = Mesh#mesh.faces
    }.

%--------------------------------------------------------------------------------------------------%

normalize_vertices( [], Normalized_Vertices ) -> Normalized_Vertices;
normalize_vertices( [{ I, { X, Y, Z } } | Vertices], Normalized_Vertices ) ->
    Length = 1 / math:sqrt( ( X * X ) + ( Y * Y ) + ( Z * Z ) ),
    Normalized_Vertex = { I , { X * Length, Y * Length, Z * Length } },
    normalize_vertices( Vertices, [Normalized_Vertex | Normalized_Vertices] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% output_mesh/2
% 
% Writes an OFF file representation of the mesh to standard output.
% First argument is the path to the output file.
% Second agument is a mesh record.
%
output_mesh( Output_File_Path, Mesh ) -> 
    {ok, Output_File } = file:open( Output_File_Path, [write] ),
    Number_of_Vertices = Mesh#mesh.number_of_vertices,
    Number_of_Faces    = Mesh#mesh.number_of_faces,
    Number_of_Edges    = Mesh#mesh.number_of_edges,
    Vertices           = Mesh#mesh.vertices,
    Faces              = Mesh#mesh.faces,
    io:fwrite( Output_File, "OFF~n", [] ),
    io:fwrite( Output_File, "~b ~b ~b ~n", [Number_of_Vertices, Number_of_Faces, Number_of_Edges] ),
    output_vertices( Output_File, Vertices ),
    output_faces( Output_File, Faces ),
    file:close( Output_File ).

output_vertices( Output_File, Vertices ) ->
    F = fun( { _, { X, Y, Z } } ) -> io:fwrite( Output_File, "  ~10f ~10f ~10f ~n", [X, Y, Z] ) end,
    lists:foreach( F, Vertices ).
    
output_faces( Output_File, Faces ) -> 
    F = fun( { _, { A, B, C } } ) -> io:fwrite( Output_File, "3 ~10b ~10b ~10b ~n", [A, B, C] ) end,
    lists:foreach( F, Faces ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mesh_to_graph( Mesh ) ->
    Vertices = Mesh#mesh.vertices,
    Faces    = Mesh#mesh.faces,
    
    % Add all mesh vertices to graph.
    G = digraph:new(),
    add_vertices( G, Vertices ),
    add_edges( G, Vertices, Faces ).
    
%--------------------------------------------------------------------------------------------------%
% Add every vertex in the vertex list to the graph.
add_vertices( Graph, [] ) -> Graph;
add_vertices( Graph, [Vertex | Vertices] ) ->
    digraph:add_vertex( Graph, Vertex ),
    add_vertices( Graph, Vertices ).

%--------------------------------------------------------------------------------------------------%
% For each face of the form {A,B,C}, add to the graph the edges: 
%   A -> B
%   B -> A
%   A -> C
%   C -> A
%   B -> C
%   C -> B

add_edges( Graph, _,        [] ) -> Graph;
add_edges( Graph, Vertices, [{ _, { A, B, C } } | Faces]) ->

    VA = get_mesh_vertex( Vertices, A ),
    VB = get_mesh_vertex( Vertices, B ),
    VC = get_mesh_vertex( Vertices, C ),
    
    add_distinct_edge( Graph, VA, VB ),
    add_distinct_edge( Graph, VA, VC ),
    add_distinct_edge( Graph, VB, VA ),
    add_distinct_edge( Graph, VB, VC ),
    add_distinct_edge( Graph, VC, VA ),
    add_distinct_edge( Graph, VC, VB ),
    add_edges( Graph, Vertices, Faces ).

%--------------------------------------------------------------------------------------------------%

add_distinct_edge( Graph, V1, V2 ) ->
    case edge_exists( Graph, V1, V2 ) of   
        false ->
            digraph:add_edge( Graph, V1, V2 );
        Edge -> Edge
    end.
    
%--------------------------------------------------------------------------------------------------%
% If an edge exists between V1 and V2, it returns it, otherwise returns false.

edge_exists( G, V1, V2 ) ->
    compare( digraph:out_edges( G, V1 ), digraph:in_edges( G, V2 ) ).

compare( Edges1, Edges2 ) ->
    case intersection( Edges1, Edges2 ) of
        []      -> false;
        [Edge]  -> Edge
    end.

intersection( S1, S2 ) ->
    intersection( S1, S2, [] ).

intersection( [],    _,  S ) -> S;
intersection( [H|T], S2, S ) ->
    case lists:member( H, S2 ) of
        true  -> intersection( T, S2, [H|S]);
        false -> intersection( T, S2, S)
    end.
