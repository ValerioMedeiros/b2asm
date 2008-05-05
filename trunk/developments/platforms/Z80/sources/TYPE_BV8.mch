MACHINE
    TYPE_BV8

SEES 
    TYPE_BIT

CONSTANTS
    BV8_INDEX
  , BV8
  , BV8_SET_BIT
  , BV8_COMPLEMENT
  , BV8_ALL_ZEROES
  , BV8_AND
  , BV8_IOR
  , BV8_XOR

PROPERTIES
    BV8_INDEX = (0..7)

  & BV8 = (BV8_INDEX --> BIT)

  & BV8_SET_BIT : (BV8 * BV8_INDEX * BIT) --> BV8
  & ! (v, i, j, b) . (v : BV8 & i : BV8_INDEX & j : BV8_INDEX & b : BIT =>
      (i /= j => BV8_SET_BIT(v, i, b)(j) = v(j)))
  & ! (v, i, b) . (v : BV8 & i : BV8_INDEX & b : BIT => BV8_SET_BIT(v, i, b)(i) = b)

  & BV8_COMPLEMENT : BV8 --> BV8
  & ! (v, i).(v : BV8 & i : BV8_INDEX => BV8_COMPLEMENT(v)(i) = BIT_FLIP(v(i)))

  & BV8_ALL_ZEROES : BV8
  & ! (i) . (i : BV8_INDEX => BV8_ALL_ZEROES(i) = 0)

  & BV8_AND : BV8 * BV8 --> BV8
  & ! (v1, v2, i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_AND(v1, v2)(i) = BIT_AND(v1(i),v2(i)))

  & BV8_IOR : BV8 * BV8 --> BV8
  & ! (v1, v2, i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_IOR(v1, v2)(i) = BIT_IOR(v1(i),v2(i)))

  & BV8_XOR : BV8 * BV8 --> BV8
  & ! (v1, v2, i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_XOR(v1, v2)(i) = BIT_XOR(v1(i),v2(i)))

ASSERTIONS
   !(vec,idx,b).(vec : BV8 & idx : BV8_INDEX & b : BIT => 
	BV8_SET_BIT(vec,idx,b)(idx) = b)
END
