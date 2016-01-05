/* Test_inv2
 * Author: Valerio
 * Creation date: 22/11/14
 */
MACHINE
    Test_inv2
CONSTANTS
    func,
    func_inv
PROPERTIES
    /*func : 0..1 --> 0..1 &  You must remove it here */
    func = %(elem).( elem : 0..1 | elem +1) &
    
    /*func_inv : 0..1 --> 0..1 &  You must remove it here */
    func_inv = %(elem).( elem : 0..1 | elem -1)
ASSERTIONS
    func : 0..1 --> 0..1 &
    func_inv : 0..1 --> 0..1
    
END