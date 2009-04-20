/*************************************************************************
Copyright (c) 2008, Valerio Medeiros Jr., David Deharbe

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* Neither the name of the Universidade Federal do Rio Grande do Norte nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*************************************************************************/

MACHINE
    Z80_Man_bitControl
INCLUDES
    MEMORY
SEES 
    ALU ,
    BIT_DEFINITION,
    BIT_VECTOR_DEFINITION,
    BYTE_DEFINITION,
    BV16_DEFINITION,
    
    UCHAR_DEFINITION,
    SCHAR_DEFINITION,
    SSHORT_DEFINITION,
    USHORT_DEFINITION,
    POWER2 
SETS
    id_reg_8 = { a0 , f0 , f_0 , a_0 ,
        b0 , c0 , b_0 , c_0 ,
        d0 , e0 , d_0 , e_0 ,
        h0 , l0 , h_0 , l_0 } ;
    id_reg_16 = { BC , DE , HL , SP , AF }        
ABSTRACT_VARIABLES
    rgs8,
    pc , sp , ix , iy ,
    i_,r_ , 
    iff1 , iff2,
    im ,
    io_ports   
INVARIANT
    rgs8 : id_reg_8 --> BYTE &
    pc : USHORT & sp : BV16 & ix : BV16 & iy : BV16 &
    i_ : BYTE & r_  : BYTE & 
    iff1 : BIT &  iff2 : BIT &
    im : (BIT*BIT) &
    io_ports  : BYTE --> BYTE
DEFINITIONS
    bv_BC == byte_bv16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ;
    bv_HL == byte_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ;
    bv_DE == byte_bv16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ;
    bv_AF == byte_bv16 ( rgs8 ( a0 ) , rgs8 ( f0 ) ) ;
    
    bv_9BC0 == mem ( byte_bv16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) );
    bv_9DE0 == mem ( byte_bv16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) );
    bv_9HL0 == mem ( byte_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) );
    bv_9AF0 == mem ( byte_bv16 ( rgs8 ( a0 ) , rgs8 ( f0 ) ) );
    
    bv_9SP0 == mem ( sp );
    bv_9IX0 == mem ( ix );
    bv_9IY0 == mem ( iy );
    
    sp_plus_one == ushort_bv16 ( add16USHORT ( 0 , bv16_ushort ( sp ) , 1 ) );
    sp_plus_two == ushort_bv16 ( add16USHORT ( 0 , bv16_ushort ( sp ) , 2 ) );
    sp_minus_one == ushort_bv16 ( sub16USHORT ( 0 , bv16_ushort ( sp ) , 1 ) );
    sp_minus_two == ushort_bv16 ( sub16USHORT ( 0 , bv16_ushort ( sp ) , 2 ) );
    
    z_s  == bitget ( rgs8 ( f0 ) , 7 ) ;
    z_z  == bitget ( rgs8 ( f0 ) , 6 ) ;
    z_00 == bitget ( rgs8 ( f0 ) , 5 ) ;
    z_h  == bitget ( rgs8 ( f0 ) , 4 ) ;
    z_01 == bitget ( rgs8 ( f0 ) , 3 ) ;
    z_p  == bitget ( rgs8 ( f0 ) , 2 ) ;
    z_n  == bitget ( rgs8 ( f0 ) , 1 ) ;
    z_c  == bitget ( rgs8 ( f0 ) , 0 )    
    
    
CONCRETE_CONSTANTS
    get_bv_reg16 ,
    REG16_TO_REG8 ,
    REG8_TO_REG16 ,
    
    update_flag_reg	 ,
    
    bv_ireg_plus_d ,
    bv_9ireg_plus_d0,
    
    cc_get
PROPERTIES
    
    
    get_bv_reg16 : ( BV16 * ( id_reg_8 --> BYTE ) * id_reg_16 ) --> ( BV16 )
    & ! ( sp_ , rgs8_ , r1 ) .
    ( sp_ :  BV16 & rgs8_ : ( id_reg_8 --> BYTE ) & r1 : id_reg_16
        => ( r1 = BC =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = byte_bv16 ( rgs8_ ( b0 ) , rgs8_ ( c0 ) ) ) &
        ( r1 = DE =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = byte_bv16 ( rgs8_ ( d0 ) , rgs8_ ( e0 ) ) ) &
        ( r1 = HL =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = byte_bv16 ( rgs8_ ( h0 ) , rgs8_ ( l0 ) ) ) &
        ( r1 = SP =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = sp_ ) &
        ( r1 = AF =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = byte_bv16 ( rgs8_ ( a0 ) , rgs8_ ( f0 ) ) )
    ) &
    
    REG16_TO_REG8 : id_reg_16 --> ( id_reg_8 * id_reg_8 ) &
    REG16_TO_REG8 ( BC ) = ( b0 , c0 )  &
    REG16_TO_REG8 ( DE ) = ( d0 , e0 )  &
    REG16_TO_REG8 ( HL ) = ( h0 , l0 )  &
    REG16_TO_REG8 ( AF ) = ( a0 , f0 ) 
    
    &
    
    REG8_TO_REG16 : ( id_reg_8 * id_reg_8 ) +-> id_reg_16 &
    REG8_TO_REG16 = REG16_TO_REG8 ~ &
    
    
    update_flag_reg : ( BIT * BIT * BIT * BIT * BIT * BIT --> ({f0} * BYTE) )&
    update_flag_reg = % ( s7 , z6 , h4 , pv2 , n_add_sub , c0 ) .
    ( s7 : BIT & z6  : BIT &   h4 : BIT  & pv2 : BIT & n_add_sub : BIT & c0 : BIT |
        ( f0 |-> [c0, n_add_sub, pv2, 1, h4, 1, z6, s7]) )
    
    &
    
    bv_ireg_plus_d : ( BV16 * SCHAR --> BV16) &
    bv_ireg_plus_d = % ( ix_iy , desloc ) . ( ix_iy : BV16 & desloc : SCHAR |
        ushort_bv16 ( (bv16_ushort ( ix_iy ) + desloc ) mod 65536 ) ) &
    
    bv_9ireg_plus_d0 : ((BV16 --> BYTE) * BV16 * SCHAR) --> BYTE  &
    bv_9ireg_plus_d0 = % ( mem , ix_iy , desloc ) . ( mem : ( BV16 --> BYTE ) & ix_iy : BV16 & desloc : SCHAR |
        mem ( bv_ireg_plus_d(ix_iy , desloc) ) ) &
    
    cc_get : ( (id_reg_8 --> BYTE) * (0..8) ) --> BIT &
    !(rgs8_).(rgs8_ : id_reg_8 --> BYTE => 	 cc_get(rgs8_ , 0) = 1 - bitget ( rgs8_ ( f0 ) , 6 )   &
        cc_get(rgs8_ , 1) =  bitget ( rgs8_ ( f0 ) , 6 )   &
        cc_get(rgs8_ , 2) =  1 - bitget ( rgs8_ ( f0 ) , 0 )   &
        cc_get(rgs8_ , 3) =   bitget ( rgs8_ ( f0 ) , 0 )   &
        cc_get(rgs8_ , 4) =  1 - bitget ( rgs8_ ( f0 ) , 2 )   &
        cc_get(rgs8_ , 5) =   bitget ( rgs8_ ( f0 ) , 2 )   & 
        cc_get(rgs8_ , 6) =  1 - bitget ( rgs8_ ( f0 ) , 7 )   & 
        cc_get(rgs8_ , 7) =  bitget ( rgs8_ ( f0 ) , 7 )   )
    
ASSERTIONS
    
    ran ( mem ) <: BYTE &
    dom ( mem ) = BV16 &
    ran ( rgs8 ) <: BYTE &
    dom ( rgs8 ) = id_reg_8 &
    
    
    instruction_next(pc) : USHORT & 
    
    
    bv_9BC0 : BYTE  & mem ( byte_bv16 ( schar_byte ( 0 ) , bv_9BC0 ) ) : BYTE &
    bv_9DE0 : BYTE  & mem ( byte_bv16 ( schar_byte ( 0 ) , bv_9DE0 ) ) : BYTE &
    bv_9HL0 : BYTE  & mem ( byte_bv16 ( schar_byte ( 0 ) , bv_9HL0 ) ) : BYTE &
    bv_9AF0 : BYTE  & mem ( byte_bv16 ( schar_byte ( 0 ) , bv_9AF0 ) ) : BYTE &
    
    
    bv_9SP0 : BYTE  &
    bv_9IX0 : BYTE  &
    bv_9IY0 : BYTE &
    
    
    update_refresh_reg(r_): BYTE &
    
    
    dom(update_flag_reg) = ( BIT * BIT * BIT  * BIT * BIT * BIT ) &
    ran(update_flag_reg) : POW({f0} * BYTE) &
    
    !(b1,b2,b3,b4,b5,b6).(  b1: BIT & b2: BIT & b3: BIT & b4: BIT & b5: BIT & b6 : BIT   => update_flag_reg(b1,b2,b3,b4,b5,b6) : {f0} * BYTE ) &
    
    !(xx).(xx: id_reg_8 -->BYTE =>  (rgs8 <+ xx) : id_reg_8 --> BYTE) &
    
    !(xx,xxr).(xx: id_reg_8 & xxr : BYTE =>  rgs8 <+ { xx|->xxr } : id_reg_8 --> BYTE) &
    
    !(xx,xxr,yy,yyr).(xx: id_reg_8 & xxr : BYTE & yy: id_reg_8 & yyr : BYTE &  not(xx=yy) 
        =>  rgs8 <+ { xx|->xxr,yy|->yyr } : id_reg_8 --> BYTE) &
    
    !(xx,xxr,yy,yyr,zz,zzr).(xx: id_reg_8 & xxr : BYTE & yy: id_reg_8 & yyr : BYTE &  zz: id_reg_8 & zzr : BYTE &
        not(xx=yy) & not(xx=zz) & not(yy=zz) 
        =>  rgs8 <+ { xx|->xxr,yy|->yyr , zz|->zzr } : id_reg_8 --> BYTE) &
    
    !(xx,xxr,yy,yyr,zz,zzr,vv,vvr).(xx: id_reg_8 & xxr : BYTE & yy: id_reg_8 & yyr : BYTE &  zz: id_reg_8 & zzr : BYTE &
        vv : id_reg_8 & vvr: BYTE &
        not(xx=yy) & not(xx=zz) & not(xx=vv) &  not(yy=zz) & not(yy=vv) & not(zz=vv) 
        =>  rgs8 <+ { xx|->xxr,yy|->yyr , zz|->zzr ,  vv|->vvr } : id_reg_8 --> BYTE) &
    
    
    !(xx,xxr,yy,yyr,zz,zzr,vv,vvr,ww,wwr).
    (xx: id_reg_8 & xxr : BYTE & yy: id_reg_8 & yyr : BYTE &  zz: id_reg_8 & zzr : BYTE &
        vv : id_reg_8 & vvr: BYTE & ww : id_reg_8 &  wwr : BYTE & not(xx=yy) & not(xx=zz) &
        not(xx=vv) & not(xx=ww) & not(yy=zz) & not(yy=vv) & not(yy=ww) & not(zz=vv) & not(zz=ww) & not(vv=ww)
        =>  rgs8 <+ { xx|->xxr,yy|->yyr , zz|->zzr ,  vv|->vvr,  ww |-> wwr } : id_reg_8 --> BYTE) &
    
    !(xx,xxr,yy,yyr,zz,zzr,vv,vvr,ww,wwr,aa,aar).
    (xx: id_reg_8 & xxr : BYTE & yy: id_reg_8 & yyr : BYTE &  zz: id_reg_8 & zzr : BYTE &
        vv : id_reg_8 & vvr: BYTE & ww : id_reg_8 &  wwr : BYTE &  aa : id_reg_8 & aar : BYTE &
        not(xx=yy) & not(xx=zz) & not(xx=vv) & not(xx=ww) & not(xx=aa) & not(yy=zz) & not(yy=vv) & not(yy=ww)
        & not(yy=aa) & not(zz=vv) & not(zz=ww) & not(zz=aa) & not(vv=ww) & not(vv=aa) & not(ww=aa)
        =>  rgs8 <+ { xx|->xxr,yy|->yyr , zz|->zzr ,  vv|->vvr,  ww |-> wwr, aa |-> aar } : id_reg_8 --> BYTE) &
    
    !(xx,xxr,yy,yyr,zz,zzr,vv,vvr,ww,wwr,aa,aar,bb,bbr).
    (xx: id_reg_8 & xxr : BYTE & yy: id_reg_8 & yyr : BYTE &  zz: id_reg_8 & zzr : BYTE &
        vv : id_reg_8 & vvr: BYTE & ww : id_reg_8 &  wwr : BYTE &  aa : id_reg_8 & aar : BYTE & 
        bb: id_reg_8 & bbr : BYTE & not(xx=yy) & not(xx=zz) & not(xx=vv) & not(xx=ww) &
        not(xx=aa) & not(xx=bb) & not(yy=zz)
        & not(yy=vv) & not(yy=ww)  & not(yy=aa) & not(yy=bb) & not(zz=vv) & not(zz=ww) & not(zz=aa) &
        not(zz=bb) & not(vv=ww) & not(vv=aa)& not(vv=bb) & not(ww=aa) & not(ww=bb) & not(aa=bb)
        =>  rgs8 <+ { xx|->xxr,yy|->yyr , zz|->zzr ,  vv|->vvr,  ww |-> wwr, aa |-> aar, bb |-> bbr }
        : id_reg_8 --> BYTE) &  
    
    
    
    dom(bv_ireg_plus_d) = ( BV16 * SCHAR ) &
    ran(bv_ireg_plus_d) = ( BV16 ) &
    !(dd).(dd: SCHAR =>  bv_ireg_plus_d(ix,dd) : BV16) &
    !(dd).(dd: SCHAR =>  bv_ireg_plus_d(iy,dd) : BV16) &
    
    
    dom(bv_9ireg_plus_d0) = ( (BV16 --> BYTE) * BV16 * SCHAR) &
    ran(bv_9ireg_plus_d0) = ( BYTE ) &
    
    !(dd).(dd: SCHAR =>  bv_9ireg_plus_d0(mem,ix,dd) : BYTE) &
    !(dd).(dd: SCHAR =>  bv_9ireg_plus_d0(mem,iy,dd) : BYTE) &
    
    /**** Define types of contants ussualy used ****/
    
    bv_BC  : BV16 &
    bv_HL : BV16 &
    bv_DE : BV16 &
    bv_AF : BV16 &
    
    bv_9BC0 : BYTE &
    bv_9DE0 : BYTE &
    bv_9HL0 : BYTE &
    bv_9AF0 : BYTE &
    
    bv_9SP0 : BYTE &
    bv_9IX0 : BYTE &
    bv_9IY0 : BYTE &
    
    
    dec_BV16(bv_BC)  : BV16 &
    inc_BV16(bv_BC)  : BV16 &
    
    dec_BV16(bv_DE)  : BV16 &
    inc_BV16(bv_DE)  : BV16 &
    
    inc_BV16(bv_HL)  : BV16 &
    dec_BV16(bv_HL)  : BV16 &
    
    sp_plus_one : BV16 &
    sp_plus_two : BV16 &
    sp_minus_one : BV16 &
    sp_minus_two : BV16 &
    
    z_s  : BIT &
    z_z  : BIT &
    z_00 : BIT &
    z_h  : BIT &
    z_01 : BIT &
    z_p  : BIT &
    z_n  : BIT &
    z_c  : BIT &
    
    
    /**** Define types of contants ussualy often ****/
    
    
    io_ports(rgs8(c0)): BYTE &
    
    dec(rgs8(b0)) : BYTE 
    
    
    
INITIALISATION
    
    rgs8 := { (a0 |-> [1,1,1,1,1,1,1,1]) , (f0 |-> [1,1,1,1,1,1,1,1]) , (f_0 |-> [1,1,1,1,1,1,1,1]) , (a_0 |-> [1,1,1,1,1,1,1,1]) ,
        (b0 |-> [1,1,1,1,1,1,1,1]) , (c0 |-> [1,1,1,1,1,1,1,1]) , (b_0 |-> [1,1,1,1,1,1,1,1]) , (c_0 |-> [1,1,1,1,1,1,1,1]) ,
        (d0 |-> [1,1,1,1,1,1,1,1]) , (e0 |-> [1,1,1,1,1,1,1,1]) , (d_0 |-> [1,1,1,1,1,1,1,1]) , (e_0 |-> [1,1,1,1,1,1,1,1]) ,
        (h0 |-> [1,1,1,1,1,1,1,1]) , (l0 |-> [1,1,1,1,1,1,1,1]) , (h_0 |-> [1,1,1,1,1,1,1,1]) , (l_0 |-> [1,1,1,1,1,1,1,1]) } ||
    
    pc := 0 || sp := [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] || ix := [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] || iy := [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] ||
    i_ :=uchar_byte(0) || r_  :=[0,0,0,0,0,0,0,0] || 
    io_ports  :: (BYTE --> {[0,0,0,0,0,0,0,0]}) ||
    iff1 :=0  ||  iff2 :=0 ||
    im := (0|->0)

OPERATIONS

    
    /************************************************************************************** 
    ***  			Rotate and Shift Group pg. 190
    **************************************************************************************/
    
    RLCA =
    BEGIN
        rgs8 := rgs8 <+ { a0 |->rotateleft(rgs8(a0)) , 
            update_flag_reg(z_s,z_z,0,z_p,0,bitget(rgs8(a0),7) )   }||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        
    END;
    
    
    RLA =
    BEGIN
        rgs8 := rgs8 <+ { a0 |-> (rotateleft(rgs8(a0)) <+ {(0|-> z_c)} ), 
            update_flag_reg(z_s,z_z,0,z_p,0,bitget(rgs8(a0),7) )   }||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        
    END;
    
    
    RRCA =
    BEGIN
        rgs8 := rgs8 <+ { a0 |-> rotateright(rgs8(a0)) , 
            update_flag_reg(z_s,z_z,0,z_p,0,bitget(rgs8(a0),0) )   }||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        
    END;
    
    
    RRA =
    BEGIN
        rgs8 := rgs8 <+ { a0 |-> (rotateright(rgs8(a0))<+ {(0|-> z_c)} ), 
            update_flag_reg(z_s,z_z,0,z_p,0,bitget(rgs8(a0),0) )   }||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        
    END;
    
    RLC_r(rr) =
    PRE rr : id_reg_8 & rr /= f0 THEN
        ANY res WHERE res : BYTE & res = rotateleft(rgs8(rr)) THEN
            
            rgs8 := rgs8 <+ { rr |-> res, 
                update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res) ,0,bitget(rgs8(rr),7) )   }||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    RLC_9HL0 =
    PRE bv16_ushort( bv_HL )  : DATA_R_ADR THEN
        ANY res WHERE res : BYTE & res = rotateleft( bv_9HL0 ) THEN
            rgs8 := rgs8 <+ { update_flag_reg(  is_negative(res), is_zero(res),0,
                    parity_even(res) ,0,bitget(bv_9HL0,7) )   }||
            updateAddressMem(bv_HL, res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END
    END;
    
    
    RLC_9IX_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort ( bv_ireg_plus_d(ix,desloc)) :  DATA_R_ADR THEN
        ANY res WHERE res : BYTE & res = rotateleft( bv_9ireg_plus_d0(mem,ix,desloc) ) THEN         
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res),
                    0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),7) )   }||            
            updateAddressMem(bv_ireg_plus_d(ix,desloc), res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    RLC_9IY_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort (bv_ireg_plus_d(iy,desloc)) :  DATA_R_ADR THEN
        ANY res WHERE res : BYTE & res = rotateleft( bv_9ireg_plus_d0(mem,iy,desloc) ) THEN         
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res),
                    0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),7) )   }||            
            updateAddressMem(bv_ireg_plus_d(iy,desloc), res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    
    
    RL_r(rr) =
    PRE rr : id_reg_8 & rr /= f0 THEN
        ANY res WHERE res : BYTE & res = (rotateleft(rgs8(rr))<+ {(0|-> z_c)} ) THEN
            
            rgs8 := rgs8 <+ { rr |-> res, 
                update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res) ,0,bitget(rgs8(rr),7) )   }||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    RL_9HL =
    PRE bv16_ushort (bv_HL) :  DATA_R_ADR THEN
        ANY res WHERE res : BYTE & res = (rotateleft( bv_9HL0 )<+ {( 0 |-> z_c)} ) THEN
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res) ,0,bitget(bv_9HL0,7) )   }||
            updateAddressMem(bv_HL, res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END
    END	;
    
    
    RL_9IX_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(ix,desloc)) :  DATA_R_ADR  THEN
        ANY res WHERE res : BYTE & res = (rotateleft( bv_9ireg_plus_d0(mem,ix,desloc) ) <+ {(0|-> z_c)} ) 			THEN
            
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res),
                    0, bitget(bv_9ireg_plus_d0(mem,ix,desloc),7) )   }||            
            updateAddressMem(bv_ireg_plus_d(ix,desloc), res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    RL_9IY_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(iy,desloc)) :  DATA_R_ADR  THEN
        ANY res WHERE res : BYTE & res = (rotateleft( bv_9ireg_plus_d0(mem,iy,desloc) )<+ {(0|-> z_c)} ) 		
        THEN
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res),
                    0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),7) )   }||            
            updateAddressMem(bv_ireg_plus_d(iy,desloc), res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    RRC_r(rr) =
    PRE rr : id_reg_8 & rr /= f0 THEN
        ANY res WHERE res : BYTE & res = rotateright(rgs8(rr)) THEN
            rgs8 := rgs8 <+ { rr |-> res, 
                update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res) ,0,bitget(rgs8(rr),0) )   }||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    RRC_9HL0 =
    PRE bv16_ushort(bv_HL) :  DATA_R_ADR THEN
        ANY res WHERE res : BYTE & res = rotateright( bv_9HL0 ) THEN
            
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res) ,0,bitget(bv_9HL0,0) )   }||
            
            updateAddressMem(bv_HL, res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END
    END;
    
    
    RRC_9IX_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(ix,desloc)) :  DATA_R_ADR  THEN
        ANY res WHERE res : BYTE & res = rotateright( bv_9ireg_plus_d0(mem,ix,desloc) ) THEN
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res),
                    0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),0) )   }||            
            updateAddressMem(bv_ireg_plus_d(ix,desloc), res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    RRC_9IY_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(iy,desloc)) :  DATA_R_ADR  THEN
        ANY res WHERE res : BYTE & res = rotateright( bv_9ireg_plus_d0(mem,iy,desloc) ) THEN
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res),
                    0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),0) )   }||            
            updateAddressMem(bv_ireg_plus_d(iy,desloc), res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    RR_r(rr) =
    PRE rr : id_reg_8 & rr /= f0 THEN
        ANY res WHERE res : BYTE & res = (rotateright(rgs8(rr))<+ {(0|-> z_c)} ) THEN
            rgs8 := rgs8 <+ { rr |-> res, 
                update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res) ,0,bitget(rgs8(rr),0) )   }||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    RR_9HL =
    PRE bv16_ushort(bv_HL) : STACK_R_ADR THEN
        ANY res WHERE res : BYTE & res = (rotateright( bv_9HL0 )<+ {(0|-> z_c)} ) THEN
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res) ,0,bitget(bv_9HL0,0) )   }||
            updateAddressMem(bv_HL, res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END
    END;
    
    
    RR_9IX_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(ix,desloc)) : DATA_R_ADR THEN
        ANY res WHERE res : BYTE & res = (rotateright( bv_9ireg_plus_d0(mem,ix,desloc) ) <+ {(0|-> z_c)} ) 			THEN
            
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res),
                    0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),0) )   }||            
            updateAddressMem(bv_ireg_plus_d(ix,desloc), res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    RR_9IY_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(iy,desloc)) : DATA_R_ADR THEN
        ANY res WHERE res : BYTE & res = (rotateright( bv_9ireg_plus_d0(mem,iy,desloc) )<+ {(0|-> z_c)} ) 		
        THEN            
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res),
                    0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),0) )   }||            
            updateAddressMem(bv_ireg_plus_d(iy,desloc), res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    SLA_r(rr) =
    PRE rr : id_reg_8 & rr /= f0 THEN
        ANY res WHERE res : BYTE & res = (rotateright(rgs8(rr))<+ {(0|-> z_c)} ) THEN
            rgs8 := rgs8 <+ { rr |-> res, 
                update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res) ,0,bitget(rgs8(rr),7) )   }||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    SLA_9HL =
    PRE  bv16_ushort(bv_HL) : STACK_R_ADR THEN
        ANY res WHERE res : BYTE & res = (rotateright( bv_9HL0 )<+ {(0|-> z_c)} ) THEN      
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res) ,0,bitget(bv_9HL0,7) )   }||
            updateAddressMem(bv_HL, res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END
    END;
    
    
    SLA_9IX_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(ix,desloc)) : DATA_R_ADR THEN
        ANY res WHERE res : BYTE & res = (rotateright( bv_9ireg_plus_d0(mem,ix,desloc) ) <+ {(0|-> z_c)} ) 			THEN
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res),
                    0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),7) )   }||            
            updateAddressMem(bv_ireg_plus_d(ix,desloc), res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    SLA_9IY_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(iy,desloc)) : DATA_R_ADR  THEN
        ANY res WHERE res : BYTE & res = (rotateright( bv_9ireg_plus_d0(mem,iy,desloc) )<+ {(0|-> z_c)} ) 		
        THEN
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res),
                    0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),7) )   }||            
            updateAddressMem(bv_ireg_plus_d(iy,desloc), res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    SRA_r(rr) =
    PRE rr : id_reg_8 & rr /= f0 THEN
        ANY res WHERE res : BYTE & res = (rotateright(rgs8(rr))) <+ {7|-> bitget(rgs8(rr),7)}  THEN
            
            rgs8 := rgs8 <+ { rr |-> res, 
                update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res) ,0,bitget(rgs8(rr),0) )   }||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    SRA_9HL0 =
    PRE  bv16_ushort(bv_HL) : DATA_R_ADR  THEN
        ANY res WHERE res : BYTE & res = (rotateright( bv_9HL0 )) <+ {7|-> bv_9HL0(7)}  THEN
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res) ,0,bitget(bv_9HL0,0) )   }||
            updateAddressMem(bv_HL, res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    SRA_9IX_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(ix,desloc)) : DATA_R_ADR  THEN
        ANY res WHERE res : BYTE & 
            res = (rotateright( bv_9ireg_plus_d0(mem,ix,desloc) ))<+ 
            {7 |-> bitget(bv_9ireg_plus_d0(mem,ix,desloc),7) }
        THEN
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res),
                    0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),0) )   }||            
            updateAddressMem(bv_ireg_plus_d(ix,desloc), res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    SRA_9IY_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(iy,desloc)) : DATA_R_ADR  THEN
        ANY res WHERE res : BYTE & 
            res = rotateright( bv_9ireg_plus_d0(mem,iy,desloc) ) <+
            {7 |-> bv_9ireg_plus_d0(mem,iy,desloc)(7) }
        THEN
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(res), is_zero(res),0, parity_even(res),
                    0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),0) )   }||            
            updateAddressMem(bv_ireg_plus_d(ix,desloc), res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    SRL_r(rr) =
    PRE rr : id_reg_8 & rr /= f0 THEN
        ANY res WHERE res : BYTE & res = bitclear(rotateright(rgs8(rr)),7)  THEN
            rgs8 := rgs8 <+ { rr |-> res, 
                update_flag_reg(
                    0, is_zero(res),0, parity_even(res) ,0,bitget(rgs8(rr),0) )   }||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    SRL_9HL0 =
    PRE  bv16_ushort(bv_HL) : DATA_R_ADR  THEN
        ANY res WHERE res : BYTE & res = bitclear(rotateright( bv_9HL0 ),7)  THEN
            rgs8 := rgs8 <+ { update_flag_reg(
                    0, is_zero(res),0, parity_even(res) ,0,bitget(bv_9HL0,0) )   }||
            updateAddressMem(bv_HL, res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END
    END;
    
    
    SRL_9IX_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(ix,desloc)) : DATA_R_ADR  THEN 
        ANY res WHERE res : BYTE & 
            res = bitclear(rotateright( bv_9ireg_plus_d0(mem,ix,desloc) ),7)
        THEN
            rgs8 := rgs8 <+ { update_flag_reg(
                    0, is_zero(res),0, parity_even(res),
                    0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),0) )   }||            
            updateAddressMem(bv_ireg_plus_d(ix,desloc), res ) ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    SRL_9IY_d0(desloc) =
    PRE desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(iy,desloc)) : DATA_R_ADR  THEN 
        ANY res WHERE res : BYTE & 
            res = bitclear(rotateright( bv_9ireg_plus_d0(mem,iy,desloc) ),7)
        THEN          
            rgs8 := rgs8 <+ { update_flag_reg(
                    0, is_zero(res),0, parity_even(res),
                    0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),0) )   }||            
            updateAddressMem(bv_ireg_plus_d(iy,desloc), res )  ||
            pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END	
    END;
    
    
    RLD=
    PRE bv16_ushort(bv_HL) : DATA_R_ADR THEN
        ANY res, acc WHERE res : BYTE & acc : BYTE &
            res =  { 7|->bitget(bv_9HL0,3), 6|->bitget(bv_9HL0,2), 5|->bitget(bv_9HL0,1), 
                4|->bitget(bv_9HL0,0),
                3|->bitget(rgs8(a0),3),   2|->bitget(rgs8(a0),2), 1|->bitget(rgs8(a0),1),
                0|->bitget(rgs8(a0),0)	} &
            acc = rgs8(a0) <+ { 3|->bitget(bv_9HL0,7),   2|->bitget(bv_9HL0,6), 
                1|->bitget(bv_9HL0,5),	0|->bitget(bv_9HL0,4)  }
            
        THEN
            
            rgs8 := rgs8 <+ { update_flag_reg(
                    is_negative(acc), is_zero(acc),0, parity_even(acc),
                    0,z_c )   }||            
            updateAddressMem(bv_HL, res ) 
            || pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END
    END;
    
    
    RRD=
    PRE bv16_ushort(bv_HL) : DATA_R_ADR THEN
        ANY res, acc WHERE res : BYTE & acc : BYTE &
            res =  { 7|->bitget(rgs8(a0),3),   6|->bitget(rgs8(a0),2), 5|->bitget(rgs8(a0),1),
                4|->bitget(rgs8(a0),0),
                3|->bitget(bv_9HL0,7), 2|->bitget(bv_9HL0,6), 1|->bitget(bv_9HL0,5), 
                0|->bitget(bv_9HL0,4)} &
            acc = rgs8(a0) <+ { 3|->bitget(bv_9HL0,3),   2|->bitget(bv_9HL0,2), 
                1|->bitget(bv_9HL0,1),	0|->bitget(bv_9HL0,0)  }
            
        THEN
            rgs8 := rgs8 <+ { update_flag_reg( is_negative(acc), is_zero(acc),0, parity_even(acc), 0 ,z_c )   }||            
            updateAddressMem(bv_HL,res) 
            || pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
        END
    END;
    
    
    /************************************************************************************** 
    ***  			BIT Manipulation  Group
    **************************************************************************************/
    
    BIT_b_rr (bb,rr) =
    PRE bb : 0..7& rr : id_reg_8 & rr /= f0 THEN
        rgs8 := rgs8 <+ {update_flag_reg( z_s, bit_not( bitget( rgs8(rr),bb) ), 1,z_p,0,z_c)}||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
    END;
    
    
    BIT_b_9HL0 (bb) =  
    PRE bb : 0..7 THEN
        rgs8 := rgs8 <+ {update_flag_reg( z_s, bit_not( bitget(bv_9HL0,bb) ), 1,z_p,0,z_c)}||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
    END;
    
    
    
    BIT_b_9IX_d0 (desloc, bb) =  
    PRE bb : 0..7 & desloc : SCHAR THEN
        rgs8 := rgs8 <+ {update_flag_reg( z_s, bit_not( bitget(bv_9ireg_plus_d0(mem,ix,desloc),bb) ), 1,z_p,0,z_c)}||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
    END;
    
    
    
    BIT_b_9IY_d0 (desloc, bb) =  
    PRE bb : 0..7 & desloc : SCHAR THEN
        rgs8 := rgs8 <+ {update_flag_reg( z_s, bit_not( bitget(bv_9ireg_plus_d0(mem,iy,desloc),bb) ), 1,z_p,0,z_c)}||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)
    END;
    
    
    
    SET_b_r(bb,rr)=
    PRE bb  : 0..7 & rr : id_reg_8 THEN
        rgs8(rr):= bitset(rgs8(rr),bb) 	||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)    
    END;
    
    
    SET_b_9HL0(bb)=
    PRE bb  : 0..7 & bv16_ushort(bv_HL) : DATA_R_ADR THEN
        updateAddressMem(bv_HL, bitset(bv_9HL0,bb))||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)    
    END;
    
    SET_b_9IX_d0(bb,desloc)=
    PRE bb  : 0..7 & desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(ix,desloc)) : DATA_R_ADR THEN
        updateAddressMem(bv_ireg_plus_d(ix,desloc), bitset(bv_9ireg_plus_d0(mem,ix,desloc),bb))||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)    
    END;
    
    SET_b_9IY_d0(bb,desloc)=
    PRE bb  : 0..7 & desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(iy,desloc)) : DATA_R_ADR THEN
        updateAddressMem(bv_ireg_plus_d(iy,desloc), bitset(bv_9ireg_plus_d0(mem,iy,desloc),bb))||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)    
    END;
    
    
    RES_b_r(bb,rr)=
    PRE bb  : 0..7 & rr : id_reg_8 & rr /= f0 THEN
        rgs8(rr):= bitclear(rgs8(rr),bb) 	 ||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)       
    END;
    
    
    RES_b_9HL0(bb)=
    PRE bb  : 0..7 & bv16_ushort(bv_HL) : DATA_R_ADR THEN
        updateAddressMem(bv_HL, bitclear(bv_9HL0,bb))||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)    
    END;
    
    RES_b_9IX_d0(bb,desloc)=
    PRE bb  : 0..7 & desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(ix,desloc)) : DATA_R_ADR THEN
        updateAddressMem(bv_ireg_plus_d(ix,desloc), bitclear(bv_9ireg_plus_d0(mem,ix,desloc),bb))||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)    
    END;
    
    RES_b_9IY_d0(bb,desloc)=
    PRE bb  : 0..7 & desloc : SCHAR & bv16_ushort(bv_ireg_plus_d(iy,desloc)) : DATA_R_ADR THEN
        updateAddressMem(bv_ireg_plus_d(iy,desloc), bitclear(bv_9ireg_plus_d0(mem,iy,desloc),bb))||
        pc := instruction_next(pc) || r_ := update_refresh_reg(r_)    
    END;
    
    /************************************************************************************** 
    ***  			JUMP GROUP
    **************************************************************************************/
    
    JP_nn(nn)=
    PRE nn : USHORT THEN
        pc:= nn || r_ := update_refresh_reg(r_)    
    END;
    
    JP_cc_nn(cc,nn)=
    PRE cc : BOOL & nn : USHORT THEN
        IF	cc = TRUE THEN    pc:= nn
        ELSE pc := instruction_next(pc) END  
        || r_ := update_refresh_reg(r_)    
    END;
    
    JR_e(ee)=
    PRE ee : SCHAR & ee-2 : SCHAR THEN
        pc := (pc + ee) mod 2**16 || r_ := update_refresh_reg(r_)    
    END;
    
    JR_C_e(ee)=
    PRE ee : SCHAR & ee-2 : SCHAR THEN
        IF z_c = 0 THEN		 pc := instruction_next(pc) 
        ELSE pc := (pc + ee) mod 2**16 END
        || r_ := update_refresh_reg(r_)    
    END;
    
    
    JR_NC_e(ee)=
    PRE ee : SCHAR & ee-2 : SCHAR THEN
        IF z_c = 1 THEN		 pc := instruction_next(pc) 
        ELSE pc := (pc + ee) mod 2**16 END
        || r_ := update_refresh_reg(r_)    
    END;
    
    JR_Z_e(ee)=
    PRE ee : SCHAR & ee-2 : SCHAR THEN
        IF z_z = 0 THEN		 pc := instruction_next(pc) 
        ELSE pc := (pc + ee) mod 2**16 END
        || r_ := update_refresh_reg(r_)    
    END;
    
    JR_NZ_e(ee)=
    PRE ee : SCHAR & ee-2 : SCHAR THEN
        IF z_z = 1 THEN		 pc := instruction_next(pc) 
        ELSE pc := (pc + ee) mod 2**16 END
        || r_ := update_refresh_reg(r_)    
    END;
    
    JP_HL=
    BEGIN
        pc := bv16_ushort(bv_HL) || r_ := update_refresh_reg(r_)    
    END;
    
    JP_IX=
    BEGIN
        pc := bv16_ushort( ix)  || r_ := update_refresh_reg(r_)    
    END;
    
    JP_IY=
    BEGIN
        pc :=  bv16_ushort( iy) || r_ := update_refresh_reg(r_)    
    END;
    
    DJNZ_e(ee)=
    PRE ee : SCHAR & ee-2 : SCHAR THEN
        
        rgs8(b0):=	dec(rgs8(b0))||
        
        IF is_zero(dec(rgs8(b0))) = 1 THEN	pc := instruction_next(pc)
        ELSE pc := (pc + ee) mod 2**16 END || r_ := update_refresh_reg(r_)    
        
    END;
    
    /************************************************************************************** 
    ***  			CALL and RETURN GROUP
    **************************************************************************************/
    
    CALL_nn (nn) =
    PRE nn : USHORT & bv16_ushort(sp_minus_two) : STACK_R_ADR  &  bv16_ushort(sp) : STACK_R_ADR THEN
        ANY high,low WHERE high : BYTE & low : BYTE & ( high , low ) = bv16_byte(ushort_bv16(pc)) &
			sp_minus_one /= sp_minus_two
        THEN
            updateStack( { ( sp_minus_one) |-> high, (sp_minus_two) |-> low } ) ||
            sp := sp_minus_two ||
            pc := nn   
        END
    END;
    
    
    CALL_cc_nn (cc,nn) =
    PRE cc : 0..8 & nn : USHORT & bv16_ushort(sp_minus_two) : STACK_R_ADR  &  bv16_ushort(sp) : STACK_R_ADR THEN
        ANY high,low WHERE high : BYTE & low : BYTE &  ( high , low ) = bv16_byte(ushort_bv16(pc)) 
        THEN
            IF cc_get(rgs8,cc) = 1 THEN
                updateStack( { ( sp_minus_one) |-> high, (sp_minus_two) |-> low } ) ||
                sp := sp_minus_two ||
                pc := nn 
            ELSE 
                pc := instruction_next(pc)
            END
            || r_ := update_refresh_reg(r_) 
        END 
    END;
    
    RET = 
    BEGIN
        pc := bv16_ushort(  byte_bv16( mem(sp_plus_one) , bv_9SP0 )) || sp := sp_plus_two || r_ := update_refresh_reg(r_) 
    END;
    
    
    RET_cc(cc) =
    PRE cc : 0..7 THEN
        IF  cc_get(rgs8,cc) = 1 THEN       pc := bv16_ushort(  byte_bv16( mem(sp_plus_one) , bv_9SP0 )) || sp := sp_plus_two 
        ELSE pc := instruction_next(pc) END
        || r_ := update_refresh_reg(r_) 
    END;
    
    
    RETI = 
    BEGIN
        pc := bv16_ushort(  byte_bv16( mem(sp_plus_one) , bv_9SP0 )) || sp := sp_plus_two || r_ := update_refresh_reg(r_) 
    END;
    
    
    RETN = 
    BEGIN
        pc := bv16_ushort(  byte_bv16( mem(sp_plus_one) , bv_9SP0 )) ||
        sp := sp_plus_two || r_ := update_refresh_reg(r_) || iff1:= iff2
    END;
    
    
    RST_p(pp) = 
    PRE pp : 0..7 &  bv16_ushort(sp_minus_two) : STACK_R_ADR  &  bv16_ushort(sp) : STACK_R_ADR  THEN
        ANY pc_l, pc_h WHERE pc_l : BYTE & pc_h : BYTE & bv16_byte(ushort_bv16(pc))= (pc_l,pc_h) 
        THEN 
            updateStack( { sp_minus_one |-> pc_h, sp_minus_two |-> pc_l } ) ||
            sp := sp_minus_two || pc := pp*8    || r_ := update_refresh_reg(r_) 
        END
    END
    
    
    
    
  






END