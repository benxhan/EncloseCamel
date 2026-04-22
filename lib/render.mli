(** [render_board board] draws a top-down ASCII view of the board.

		Expected behavior:
		- Print a full grid to standard output for the provided board dimensions.
		- Render empty cells and rock cells with visually distinct characters.
		- Include coordinate labels (or equivalent orientation hints) so users can
			choose valid [x,y] inputs.
		- Avoid mutating any game state; rendering is display-only.
*)
val render_board : Model.board -> unit

(** [print_place_result result] prints feedback for failed placements.

		Expected behavior:
		- For [Out_of_bounds], explain that the coordinate is outside the board.
		- For [Occupied], explain that the target cell already has a rock.
		- For [Ok _], print nothing because success feedback can be handled by the
			caller (for example the main loop).
*)
val print_place_result : Model.place_result -> unit
