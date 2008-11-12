/*
Colocar uma definição de BV_INDX(bv) = 1..bv_size(bv) tal que bv:BIT_VECTOR
*/
MACHINE
        BIT_VECTOR_DEFINITION

SEES
        BIT_DEFINITION

CONCRETE_CONSTANTS

        BV_INDX , BIT_VECTOR ,
        bv_catenate , bv_sub , bv_zero , bv_one ,
        bv_size , bv_not ,     bv_and ,       bv_or , bv_xor ,
        bv_at , bv_set , bv_clear , bv_put

PROPERTIES


           BIT_VECTOR = { bt | bt : NATURAL +-> BIT } - { {} } &

        bv_size : BIT_VECTOR --> NATURAL1 &
        bv_size = % ( bv ) . ( bv : BIT_VECTOR | size ( bv ) ) &

        BV_INDX : BIT_VECTOR --> POW ( NATURAL )     &
        BV_INDX = % bv . ( bv : BIT_VECTOR | 0 .. ( bv_size ( bv ) - 1 ) ) &

        bv_catenate : BIT_VECTOR * BIT_VECTOR --> BIT_VECTOR &
        bv_catenate = % ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR | v1 ^ v2 ) &

        bv_sub : BIT_VECTOR * NATURAL * NATURAL --> BIT_VECTOR &
        bv_sub = % ( bv , low , high ) . ( bv : BIT_VECTOR & low : BV_INDX ( bv ) & high : BV_INDX ( bv ) & low <= high | low .. high <| bv ) &

        bv_zero : NATURAL1 --> BIT_VECTOR &
        bv_zero = % ( sz ) . ( sz : NATURAL1 | ( 0 .. sz ) * { 0 } ) &

        bv_one : NATURAL1 --> BIT_VECTOR &
        bv_one = % ( sz ) . ( sz : NATURAL1 | ( 0 .. sz ) * { 1 } ) &

        bv_not : BIT_VECTOR --> BIT_VECTOR &
        bv_not = % ( v1 ) . ( v1 : BIT_VECTOR | % ( idx ) . ( idx : BV_INDX ( v1 ) | bit_not ( v1 ( idx ) ) ) ) &

        bv_and : BIT_VECTOR * BIT_VECTOR --> BIT_VECTOR &
        bv_and = % ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) |
                % ( idx ) . ( idx : BV_INDX ( v1 ) | bit_and ( v1 ( idx ) , v2 ( idx ) ) ) ) &

        bv_or : BIT_VECTOR * BIT_VECTOR --> BIT_VECTOR &
        bv_or = % ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) |
                % ( idx ) . ( idx : BV_INDX ( v1 ) | bit_or ( v1 ( idx ) , v2 ( idx ) ) ) ) &

        bv_xor : BIT_VECTOR * BIT_VECTOR --> BIT_VECTOR &
        bv_xor = % ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) |
                % ( idx ) . ( idx : BV_INDX ( v1 ) | bit_xor ( v1 ( idx ) , v2 ( idx ) ) ) ) &

        bv_at : BIT_VECTOR * NATURAL --> BIT &
        bv_at = % ( v1 , idx ) . ( v1 : BIT_VECTOR & idx : BV_INDX ( v1 ) | v1 ( idx ) ) &

        bv_set : BIT_VECTOR * NATURAL --> BIT_VECTOR &
        bv_set = % ( v1 , idx ) . ( v1 : BIT_VECTOR & idx : BV_INDX ( v1 ) | v1 <+ { idx |-> 1 } ) &
        bv_clear : BIT_VECTOR * NATURAL --> BIT_VECTOR &
        bv_clear = % ( v1 , idx ) . ( v1 : BIT_VECTOR & idx : BV_INDX ( v1 ) | v1 <+ { idx |-> 0 } ) &

        bv_put : BIT_VECTOR * NATURAL * BIT --> BIT_VECTOR &
        bv_put = % ( v1 , idx , bit ) . ( v1 : BIT_VECTOR & idx : BV_INDX ( v1 ) & bit : BIT | v1 <+ { idx |-> bit } )



ASSERTIONS
        ! bv . ( bv : BIT_VECTOR => bv_size ( bv_not ( bv ) ) = bv_size ( bv ) ) ;

        ! ( bv , indx ) . ( bv : BIT_VECTOR & indx : BV_INDX ( bv ) => ( bv_not ( bv_not ( bv ) ) ( indx ) = bv ( indx ) ) ) ;

        ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR =>
                        bv_size ( bv_catenate ( v1 , v2 ) ) = bv_size ( v1 ) + bv_size ( v2 ) ) ;

        ! ( bv , low , high ) . ( bv : BIT_VECTOR & low : BV_INDX ( bv ) & high : BV_INDX ( bv ) & low <= high =>
                        bv_size ( bv_sub ( bv , low , high ) ) = high - low ) ;

        ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) =>
                        bv_size ( bv_and ( v1 , v2 ) ) = bv_size ( v2 ) ) ;

        ! ( v1 , v2 , indx ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) & indx : BV_INDX ( v1 ) =>
                        ( bv_and ( v1 , v2 ) ( indx ) = bv_and ( v2 , v1 ) ( indx ) ) ) ;

        ! ( v1 , v2 , v3 , indx ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & v3 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) & bv_size ( v1 ) = bv_size ( v3 ) & indx : BV_INDX ( v1 ) =>
                        ( bv_and ( v1 , bv_and ( v2 , v3 ) ) ( indx ) = bv_and ( bv_and ( v1 , v2 ) , v3 ) ( indx ) ) ) ;

        ! ( bv , indx ) . ( bv : BIT_VECTOR & indx : BV_INDX ( bv ) => ( bv_and ( bv , bv_zero ( bv_size ( bv ) ) ) ( indx ) = bv_zero ( bv_size ( bv ) ) ( indx ) ) ) ;
        ! ( bv , indx ) . ( bv : BIT_VECTOR & indx : BV_INDX ( bv ) => ( bv_and ( bv , bv_one ( bv_size ( bv ) ) ) ( indx ) = bv ( indx ) ) ) ;

        ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) =>
                        bv_size ( bv_or ( v1 , v2 ) ) = bv_size ( v1 ) ) ;

        ! ( v1 , v2 , indx ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) & indx : BV_INDX ( v1 ) =>
                        ( bv_or ( v1 , v2 ) ( indx ) = bv_or ( v2 , v1 ) ( indx ) ) ) ;

        ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) =>
                        bv_size ( bv_or ( v1 , v2 ) ) = bv_size ( v2 ) ) ; /* Redundância ????? */

        ! ( v1 , v2 , v3 , indx ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & v3 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) & bv_size ( v1 ) = bv_size ( v3 ) & indx : BV_INDX ( v1 ) => ( bv_or ( v1 , bv_or ( v2 , v3 ) ) ( indx ) = bv_or ( bv_or ( v1 , v2 ) , v3 ) ( indx ) ) ) ;

        ! ( bv , indx ) . ( bv : BIT_VECTOR & indx : BV_INDX ( bv ) => ( bv_or ( bv , bv_one ( bv_size ( bv ) ) ) ( indx ) = bv_one ( bv_size ( bv ) ) ( indx ) ) ) ;
        ! ( bv , indx ) . ( bv : BIT_VECTOR & indx : BV_INDX ( bv ) => ( bv_or ( bv , bv_zero ( bv_size ( bv ) ) ) ( indx ) = bv ( indx ) ) ) ;

        ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) =>
                        bv_size ( bv_xor ( v1 , v2 ) ) = bv_size ( v1 ) ) ;
        ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) =>
                        bv_size ( bv_xor ( v1 , v2 ) ) = bv_size ( v2 ) ) ;
        ! ( v1 , v2 , indx ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) & indx : BV_INDX ( v1 ) =>
                        ( bv_xor ( v1 , v2 ) ( indx ) = bv_xor ( v2 , v1 ) ( indx ) ) ) ;
        ! ( bv , indx ) . ( bv : BIT_VECTOR & indx : BV_INDX ( bv ) => bv_xor ( bv , bv ) ( indx ) = bv_zero ( bv_size ( bv ) ) ( indx ) )
END
