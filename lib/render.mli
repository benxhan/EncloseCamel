(** [render_board board] draws a top-down ASCII view of the board.

    Expected behavior:
    - Print a full grid to standard output for the provided board dimensions.
    - Render empty cells and rock cells with visually distinct characters.
    - Include coordinate labels (or equivalent orientation hints) so users can
      choose valid [r,c] inputs.
    - Avoid mutating any game state; rendering is display-only. *)
val render_board : Model.board -> unit

(** Baseline texel grid (64); live GUI sizing is {!gui_tile_px}. *)
val base_gui_tile_px : int

val gui_tile_px : unit -> int
val set_gui_tile_px : int -> unit

(** Legacy alias for {!base_gui_tile_px}. *)
val tile_size : int

val info_panel_height : int

val init_gui_textures : unit -> unit

val unload_gui_textures : unit -> unit

val draw_board_gui : ?offset_x:int -> ?offset_y:int -> Model.board -> unit

val str_render_board : Model.board -> string

(** [print_place_result result] prints feedback for failed placements.

    Expected behavior:
    - For [Out_of_bounds], explain that the coordinate is outside the board.
    - For [Occupied], explain that the target cell can't have a rock (water or
      caml).
    - For [Ok], print nothing because success feedback can be handled by the
      caller (for example the main loop). *)
val print_place_result : Model.place_result -> unit
