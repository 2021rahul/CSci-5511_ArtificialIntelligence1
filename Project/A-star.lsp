( defun count_wrong_w_dist ( state goal )
    "Counts the Manhattan distance of the
    tiles out of place and divides by two."
    ( let
        (
            ( count 0 ) ; The number of tiles out of place
            ( puz-size ( isqrt ( length state ) ) ) ; Side length of the puzzle
            correct-pos ; The correct position of a tile
        )
        
        ( cond

            ; Catch for if the state is an inappropriate length
            ( ( /= ( length state ) ( length goal ) )
                NIL
            )

            ( t
                ; For i = 0 .. length of state
                ( do
                    (
                        ( i 0 ( 1+ i ) )
                    )
                    ; One move will change the place of two
                    ; tiles, so divide the count by two
                    ( ( >= i ( length state ) ) ( / count 2 ) )

                    ; If a tile is out of place:
                    ( when ( not ( eq ( nth i state ) ( nth i goal ) ) )

                        ; Find the correct position
                        ( setf correct-pos ( position ( nth i state ) goal ) )

                        ; Increment count by the number of rows off
                        ( setf count ( + count
                            ( abs ( -
                                ( floor i puz-size )
                                ( floor correct-pos puz-size )
                            ) )
                        ) )

                        ; Increment count by the number of columns off
                        ( setf count ( + count
                            ( abs ( -
                                ( mod i puz-size )
                                ( mod correct-pos puz-size )
                            ) )
                        ) )
                    )
                )
            )
        )
    )
)