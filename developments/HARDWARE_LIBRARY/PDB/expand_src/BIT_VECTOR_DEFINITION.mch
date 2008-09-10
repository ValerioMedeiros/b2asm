MACHINE
    BIT_VECTOR_DEFINITION

SEES
    BIT_DEFINITION

CONCRETE_CONSTANTS
    BIT_VECTOR ,
    bv_size , bv_catenate , bv_sub , bv_zero , bv_one ,
    bv_not ,   bv_and ,   bv_or , bv_xor ,
    bv_at , bv_set , bv_clear

PROPERTIES
    BIT_VECTOR = seq1 ( BIT ) &

    bv_size : BIT_VECTOR --> NATURAL1 &
    bv_size = % ( bv ) . ( bv : BIT_VECTOR | size ( bv ) ) &

    bv_catenate : BIT_VECTOR * BIT_VECTOR --> BIT_VECTOR &
    bv_catenate = % ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR | v2 ^ v1 ) &

    bv_sub : BIT_VECTOR * NATURAL * NATURAL --> BIT_VECTOR &
    bv_sub = % ( bv , low , high ) . ( bv : BIT_VECTOR & low : NATURAL & high : NATURAL & low <= high | 0 .. ( high - low ) <| bv ) &

    bv_zero : NATURAL1 --> BIT_VECTOR &
    bv_zero = % ( sz ) . ( sz : NATURAL1 | ( 1 .. sz ) * { 0 } ) &

    bv_one : NATURAL1 --> BIT_VECTOR &
    bv_one = % ( sz ) . ( sz : NATURAL1 | ( 1 .. sz ) * { 1 } ) &

    bv_not : BIT_VECTOR --> BIT_VECTOR &
    bv_not = % ( v1 ) . ( v1 : BIT_VECTOR | % ( idx ) . ( idx : 1 .. size ( v1 ) | bit_not ( v1 ( idx ) ) ) ) &

    bv_and : BIT_VECTOR * BIT_VECTOR --> BIT_VECTOR &
    bv_and = % ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & size ( v1 ) = size ( v2 ) |
        % ( idx ) . ( idx : 1 .. size ( v1 ) | bit_and ( v1 ( idx ) , v2 ( idx ) ) ) ) &

    bv_or : BIT_VECTOR * BIT_VECTOR --> BIT_VECTOR &
    bv_or = % ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & size ( v1 ) = size ( v2 ) |
        % ( idx ) . ( idx : 1 .. size ( v1 ) | bit_or ( v1 ( idx ) , v2 ( idx ) ) ) ) &

    bv_xor : BIT_VECTOR * BIT_VECTOR --> BIT_VECTOR &
    bv_xor = % ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & size ( v1 ) = size ( v2 ) |
        % ( idx ) . ( idx : 1 .. size ( v1 ) | bit_xor ( v1 ( idx ) , v2 ( idx ) ) ) ) &

    bv_at : BIT_VECTOR * NATURAL --> BIT &
    bv_at = % ( v1 , idx ) . ( v1 : BIT_VECTOR & idx : NATURAL & idx < size ( v1 ) | v1 ( idx + 1 ) ) &

    bv_set : BIT_VECTOR * NATURAL --> BIT_VECTOR &
    bv_set = % ( v1 , idx ) . ( v1 : BIT_VECTOR & idx : NATURAL & idx < size ( v1 ) | v1 <+ { idx + 1 |-> 1 } ) &
    bv_clear : BIT_VECTOR * NATURAL --> BIT_VECTOR &
    bv_clear = % ( v1 , idx ) . ( v1 : BIT_VECTOR & idx : NATURAL & idx < size ( v1 ) | v1 <+ { idx + 1 |-> 0 } )

ASSERTIONS
    ! bv . ( bv : BIT_VECTOR => bv_size ( bv_not ( bv ) ) = bv_size ( bv ) ) ;
    ! bv . ( bv : BIT_VECTOR => ( bv_not ( bv_not ( bv ) ) = bv ) ) ;

    ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR =>
            bv_size ( bv_catenate ( v1 , v2 ) ) = bv_size ( v1 ) + bv_size ( v2 ) ) ;

    ! ( bv , low , high ) . ( bv : BIT_VECTOR & low : NATURAL & high : NATURAL & low <= high =>
            bv_size ( bv_sub ( bv , low , high ) ) = 1 + high - low ) ;

    ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) =>
            bv_size ( bv_and ( v1 , v2 ) ) = bv_size ( v2 ) ) ;
    ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) =>
            ( bv_and ( v1 , v2 ) = bv_and ( v2 , v1 ) ) ) ;
    ! ( v1 , v2 , v3 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & v3 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) & bv_size ( v1 ) = bv_size ( v3 ) =>
            ( bv_and ( v1 , bv_and ( v2 , v3 ) ) = bv_and ( bv_and ( v1 , v2 ) , v3 ) ) ) ;
    ! bv . ( bv : BIT_VECTOR => ( bv_and ( bv , bv_zero ( bv_size ( bv ) ) ) = bv_zero ( bv_size ( bv ) ) ) ) ;
    ! bv . ( bv : BIT_VECTOR => ( bv_and ( bv , bv_one ( bv_size ( bv ) ) ) = bv ) ) ;

    ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) =>
            bv_size ( bv_or ( v1 , v2 ) ) = bv_size ( v1 ) ) ;
    ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) =>
            bv_size ( bv_or ( v1 , v2 ) ) = bv_size ( v2 ) ) ;
    ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) =>
            ( bv_or ( v1 , v2 ) = bv_or ( v2 , v1 ) ) ) ;
    ! ( v1 , v2 , v3 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & v3 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) & bv_size ( v1 ) = bv_size ( v3 ) =>
            ( bv_or ( v1 , bv_or ( v2 , v3 ) ) = bv_or ( bv_or ( v1 , v2 ) , v3 ) ) ) ;
    ! bv . ( bv : BIT_VECTOR => ( bv_or ( bv , bv_one ( bv_size ( bv ) ) ) = bv_one ( bv_size ( bv ) ) ) ) ;
    ! bv . ( bv : BIT_VECTOR => ( bv_or ( bv , bv_zero ( bv_size ( bv ) ) ) = bv ) ) ;

    ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) =>
            bv_size ( bv_xor ( v1 , v2 ) ) = bv_size ( v1 ) ) ;
    ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) =>
            bv_size ( bv_xor ( v1 , v2 ) ) = bv_size ( v2 ) ) ;
    ! ( v1 , v2 ) . ( v1 : BIT_VECTOR & v2 : BIT_VECTOR & bv_size ( v1 ) = bv_size ( v2 ) =>
            ( bv_xor ( v1 , v2 ) = bv_xor ( v2 , v1 ) ) ) ;
    ! ( bv ) . ( bv : BIT_VECTOR => bv_xor ( bv , bv ) = bv_zero ( bv_size ( bv ) ) )
END
