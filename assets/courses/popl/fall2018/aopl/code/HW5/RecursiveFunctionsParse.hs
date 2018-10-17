{-# OPTIONS_GHC -w #-}
module RecursiveFunctionsParse where
import Prelude hiding (LT, GT, EQ, id)
import Data.Char
import RecursiveFunctions
import Lexer
import Operators
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12

action_0 (13) = happyShift action_2
action_0 (14) = happyShift action_10
action_0 (16) = happyShift action_11
action_0 (17) = happyShift action_12
action_0 (18) = happyShift action_13
action_0 (19) = happyShift action_14
action_0 (21) = happyShift action_15
action_0 (22) = happyShift action_16
action_0 (25) = happyShift action_17
action_0 (34) = happyShift action_18
action_0 (36) = happyShift action_19
action_0 (4) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 (11) = happyGoto action_8
action_0 (12) = happyGoto action_9
action_0 _ = happyFail

action_1 (13) = happyShift action_2
action_1 _ = happyFail

action_2 (36) = happyShift action_40
action_2 _ = happyFail

action_3 (41) = happyAccept
action_3 _ = happyFail

action_4 (35) = happyShift action_39
action_4 _ = happyReduce_5

action_5 (33) = happyShift action_38
action_5 _ = happyReduce_10

action_6 (28) = happyShift action_33
action_6 (29) = happyShift action_34
action_6 (30) = happyShift action_35
action_6 (31) = happyShift action_36
action_6 (32) = happyShift action_37
action_6 _ = happyReduce_12

action_7 (24) = happyShift action_31
action_7 (25) = happyShift action_32
action_7 _ = happyReduce_18

action_8 (26) = happyShift action_29
action_8 (27) = happyShift action_30
action_8 _ = happyReduce_21

action_9 (36) = happyShift action_28
action_9 _ = happyReduce_24

action_10 (36) = happyShift action_27
action_10 _ = happyFail

action_11 _ = happyReduce_27

action_12 _ = happyReduce_28

action_13 (21) = happyShift action_26
action_13 (5) = happyGoto action_24
action_13 (6) = happyGoto action_25
action_13 _ = happyFail

action_14 (21) = happyShift action_23
action_14 _ = happyFail

action_15 _ = happyReduce_31

action_16 _ = happyReduce_26

action_17 (16) = happyShift action_11
action_17 (17) = happyShift action_12
action_17 (21) = happyShift action_15
action_17 (22) = happyShift action_16
action_17 (25) = happyShift action_17
action_17 (34) = happyShift action_18
action_17 (36) = happyShift action_19
action_17 (12) = happyGoto action_22
action_17 _ = happyFail

action_18 (16) = happyShift action_11
action_18 (17) = happyShift action_12
action_18 (21) = happyShift action_15
action_18 (22) = happyShift action_16
action_18 (25) = happyShift action_17
action_18 (34) = happyShift action_18
action_18 (36) = happyShift action_19
action_18 (12) = happyGoto action_21
action_18 _ = happyFail

action_19 (13) = happyShift action_2
action_19 (14) = happyShift action_10
action_19 (16) = happyShift action_11
action_19 (17) = happyShift action_12
action_19 (18) = happyShift action_13
action_19 (19) = happyShift action_14
action_19 (21) = happyShift action_15
action_19 (22) = happyShift action_16
action_19 (25) = happyShift action_17
action_19 (34) = happyShift action_18
action_19 (36) = happyShift action_19
action_19 (4) = happyGoto action_20
action_19 (7) = happyGoto action_4
action_19 (8) = happyGoto action_5
action_19 (9) = happyGoto action_6
action_19 (10) = happyGoto action_7
action_19 (11) = happyGoto action_8
action_19 (12) = happyGoto action_9
action_19 _ = happyFail

action_20 (37) = happyShift action_59
action_20 _ = happyFail

action_21 (36) = happyShift action_28
action_21 _ = happyReduce_30

action_22 (36) = happyShift action_28
action_22 _ = happyReduce_29

action_23 (23) = happyShift action_58
action_23 _ = happyFail

action_24 (40) = happyShift action_57
action_24 _ = happyReduce_7

action_25 (20) = happyShift action_56
action_25 _ = happyFail

action_26 (23) = happyShift action_55
action_26 _ = happyFail

action_27 (13) = happyShift action_2
action_27 (14) = happyShift action_10
action_27 (16) = happyShift action_11
action_27 (17) = happyShift action_12
action_27 (18) = happyShift action_13
action_27 (19) = happyShift action_14
action_27 (21) = happyShift action_15
action_27 (22) = happyShift action_16
action_27 (25) = happyShift action_17
action_27 (34) = happyShift action_18
action_27 (36) = happyShift action_19
action_27 (4) = happyGoto action_54
action_27 (7) = happyGoto action_4
action_27 (8) = happyGoto action_5
action_27 (9) = happyGoto action_6
action_27 (10) = happyGoto action_7
action_27 (11) = happyGoto action_8
action_27 (12) = happyGoto action_9
action_27 _ = happyFail

action_28 (13) = happyShift action_2
action_28 (14) = happyShift action_10
action_28 (16) = happyShift action_11
action_28 (17) = happyShift action_12
action_28 (18) = happyShift action_13
action_28 (19) = happyShift action_14
action_28 (21) = happyShift action_15
action_28 (22) = happyShift action_16
action_28 (25) = happyShift action_17
action_28 (34) = happyShift action_18
action_28 (36) = happyShift action_19
action_28 (4) = happyGoto action_53
action_28 (7) = happyGoto action_4
action_28 (8) = happyGoto action_5
action_28 (9) = happyGoto action_6
action_28 (10) = happyGoto action_7
action_28 (11) = happyGoto action_8
action_28 (12) = happyGoto action_9
action_28 _ = happyFail

action_29 (16) = happyShift action_11
action_29 (17) = happyShift action_12
action_29 (21) = happyShift action_15
action_29 (22) = happyShift action_16
action_29 (25) = happyShift action_17
action_29 (34) = happyShift action_18
action_29 (36) = happyShift action_19
action_29 (12) = happyGoto action_52
action_29 _ = happyFail

action_30 (16) = happyShift action_11
action_30 (17) = happyShift action_12
action_30 (21) = happyShift action_15
action_30 (22) = happyShift action_16
action_30 (25) = happyShift action_17
action_30 (34) = happyShift action_18
action_30 (36) = happyShift action_19
action_30 (12) = happyGoto action_51
action_30 _ = happyFail

action_31 (16) = happyShift action_11
action_31 (17) = happyShift action_12
action_31 (21) = happyShift action_15
action_31 (22) = happyShift action_16
action_31 (25) = happyShift action_17
action_31 (34) = happyShift action_18
action_31 (36) = happyShift action_19
action_31 (11) = happyGoto action_50
action_31 (12) = happyGoto action_9
action_31 _ = happyFail

action_32 (16) = happyShift action_11
action_32 (17) = happyShift action_12
action_32 (21) = happyShift action_15
action_32 (22) = happyShift action_16
action_32 (25) = happyShift action_17
action_32 (34) = happyShift action_18
action_32 (36) = happyShift action_19
action_32 (11) = happyGoto action_49
action_32 (12) = happyGoto action_9
action_32 _ = happyFail

action_33 (16) = happyShift action_11
action_33 (17) = happyShift action_12
action_33 (21) = happyShift action_15
action_33 (22) = happyShift action_16
action_33 (25) = happyShift action_17
action_33 (34) = happyShift action_18
action_33 (36) = happyShift action_19
action_33 (10) = happyGoto action_48
action_33 (11) = happyGoto action_8
action_33 (12) = happyGoto action_9
action_33 _ = happyFail

action_34 (16) = happyShift action_11
action_34 (17) = happyShift action_12
action_34 (21) = happyShift action_15
action_34 (22) = happyShift action_16
action_34 (25) = happyShift action_17
action_34 (34) = happyShift action_18
action_34 (36) = happyShift action_19
action_34 (10) = happyGoto action_47
action_34 (11) = happyGoto action_8
action_34 (12) = happyGoto action_9
action_34 _ = happyFail

action_35 (16) = happyShift action_11
action_35 (17) = happyShift action_12
action_35 (21) = happyShift action_15
action_35 (22) = happyShift action_16
action_35 (25) = happyShift action_17
action_35 (34) = happyShift action_18
action_35 (36) = happyShift action_19
action_35 (10) = happyGoto action_46
action_35 (11) = happyGoto action_8
action_35 (12) = happyGoto action_9
action_35 _ = happyFail

action_36 (16) = happyShift action_11
action_36 (17) = happyShift action_12
action_36 (21) = happyShift action_15
action_36 (22) = happyShift action_16
action_36 (25) = happyShift action_17
action_36 (34) = happyShift action_18
action_36 (36) = happyShift action_19
action_36 (10) = happyGoto action_45
action_36 (11) = happyGoto action_8
action_36 (12) = happyGoto action_9
action_36 _ = happyFail

action_37 (16) = happyShift action_11
action_37 (17) = happyShift action_12
action_37 (21) = happyShift action_15
action_37 (22) = happyShift action_16
action_37 (25) = happyShift action_17
action_37 (34) = happyShift action_18
action_37 (36) = happyShift action_19
action_37 (10) = happyGoto action_44
action_37 (11) = happyGoto action_8
action_37 (12) = happyGoto action_9
action_37 _ = happyFail

action_38 (16) = happyShift action_11
action_38 (17) = happyShift action_12
action_38 (21) = happyShift action_15
action_38 (22) = happyShift action_16
action_38 (25) = happyShift action_17
action_38 (34) = happyShift action_18
action_38 (36) = happyShift action_19
action_38 (9) = happyGoto action_43
action_38 (10) = happyGoto action_7
action_38 (11) = happyGoto action_8
action_38 (12) = happyGoto action_9
action_38 _ = happyFail

action_39 (16) = happyShift action_11
action_39 (17) = happyShift action_12
action_39 (21) = happyShift action_15
action_39 (22) = happyShift action_16
action_39 (25) = happyShift action_17
action_39 (34) = happyShift action_18
action_39 (36) = happyShift action_19
action_39 (8) = happyGoto action_42
action_39 (9) = happyGoto action_6
action_39 (10) = happyGoto action_7
action_39 (11) = happyGoto action_8
action_39 (12) = happyGoto action_9
action_39 _ = happyFail

action_40 (21) = happyShift action_41
action_40 _ = happyFail

action_41 (37) = happyShift action_66
action_41 _ = happyFail

action_42 (33) = happyShift action_38
action_42 _ = happyReduce_9

action_43 (28) = happyShift action_33
action_43 (29) = happyShift action_34
action_43 (30) = happyShift action_35
action_43 (31) = happyShift action_36
action_43 (32) = happyShift action_37
action_43 _ = happyReduce_11

action_44 (24) = happyShift action_31
action_44 (25) = happyShift action_32
action_44 _ = happyReduce_13

action_45 (24) = happyShift action_31
action_45 (25) = happyShift action_32
action_45 _ = happyReduce_17

action_46 (24) = happyShift action_31
action_46 (25) = happyShift action_32
action_46 _ = happyReduce_16

action_47 (24) = happyShift action_31
action_47 (25) = happyShift action_32
action_47 _ = happyReduce_15

action_48 (24) = happyShift action_31
action_48 (25) = happyShift action_32
action_48 _ = happyReduce_14

action_49 (26) = happyShift action_29
action_49 (27) = happyShift action_30
action_49 _ = happyReduce_20

action_50 (26) = happyShift action_29
action_50 (27) = happyShift action_30
action_50 _ = happyReduce_19

action_51 (36) = happyShift action_28
action_51 _ = happyReduce_23

action_52 (36) = happyShift action_28
action_52 _ = happyReduce_22

action_53 (37) = happyShift action_65
action_53 _ = happyFail

action_54 (37) = happyShift action_64
action_54 _ = happyFail

action_55 (13) = happyShift action_2
action_55 (14) = happyShift action_10
action_55 (16) = happyShift action_11
action_55 (17) = happyShift action_12
action_55 (18) = happyShift action_13
action_55 (19) = happyShift action_14
action_55 (21) = happyShift action_15
action_55 (22) = happyShift action_16
action_55 (25) = happyShift action_17
action_55 (34) = happyShift action_18
action_55 (36) = happyShift action_19
action_55 (4) = happyGoto action_63
action_55 (7) = happyGoto action_4
action_55 (8) = happyGoto action_5
action_55 (9) = happyGoto action_6
action_55 (10) = happyGoto action_7
action_55 (11) = happyGoto action_8
action_55 (12) = happyGoto action_9
action_55 _ = happyFail

action_56 (13) = happyShift action_2
action_56 (14) = happyShift action_10
action_56 (16) = happyShift action_11
action_56 (17) = happyShift action_12
action_56 (18) = happyShift action_13
action_56 (19) = happyShift action_14
action_56 (21) = happyShift action_15
action_56 (22) = happyShift action_16
action_56 (25) = happyShift action_17
action_56 (34) = happyShift action_18
action_56 (36) = happyShift action_19
action_56 (4) = happyGoto action_62
action_56 (7) = happyGoto action_4
action_56 (8) = happyGoto action_5
action_56 (9) = happyGoto action_6
action_56 (10) = happyGoto action_7
action_56 (11) = happyGoto action_8
action_56 (12) = happyGoto action_9
action_56 _ = happyFail

action_57 (21) = happyShift action_26
action_57 (5) = happyGoto action_24
action_57 (6) = happyGoto action_61
action_57 _ = happyFail

action_58 (13) = happyShift action_2
action_58 (14) = happyShift action_10
action_58 (16) = happyShift action_11
action_58 (17) = happyShift action_12
action_58 (18) = happyShift action_13
action_58 (19) = happyShift action_14
action_58 (21) = happyShift action_15
action_58 (22) = happyShift action_16
action_58 (25) = happyShift action_17
action_58 (34) = happyShift action_18
action_58 (36) = happyShift action_19
action_58 (4) = happyGoto action_60
action_58 (7) = happyGoto action_4
action_58 (8) = happyGoto action_5
action_58 (9) = happyGoto action_6
action_58 (10) = happyGoto action_7
action_58 (11) = happyGoto action_8
action_58 (12) = happyGoto action_9
action_58 _ = happyFail

action_59 _ = happyReduce_32

action_60 (20) = happyShift action_69
action_60 _ = happyFail

action_61 _ = happyReduce_8

action_62 _ = happyReduce_2

action_63 _ = happyReduce_6

action_64 (13) = happyShift action_2
action_64 (14) = happyShift action_10
action_64 (16) = happyShift action_11
action_64 (17) = happyShift action_12
action_64 (18) = happyShift action_13
action_64 (19) = happyShift action_14
action_64 (21) = happyShift action_15
action_64 (22) = happyShift action_16
action_64 (25) = happyShift action_17
action_64 (34) = happyShift action_18
action_64 (36) = happyShift action_19
action_64 (4) = happyGoto action_68
action_64 (7) = happyGoto action_4
action_64 (8) = happyGoto action_5
action_64 (9) = happyGoto action_6
action_64 (10) = happyGoto action_7
action_64 (11) = happyGoto action_8
action_64 (12) = happyGoto action_9
action_64 _ = happyFail

action_65 _ = happyReduce_25

action_66 (38) = happyShift action_67
action_66 _ = happyFail

action_67 (13) = happyShift action_2
action_67 (14) = happyShift action_10
action_67 (16) = happyShift action_11
action_67 (17) = happyShift action_12
action_67 (18) = happyShift action_13
action_67 (19) = happyShift action_14
action_67 (21) = happyShift action_15
action_67 (22) = happyShift action_16
action_67 (25) = happyShift action_17
action_67 (34) = happyShift action_18
action_67 (36) = happyShift action_19
action_67 (4) = happyGoto action_72
action_67 (7) = happyGoto action_4
action_67 (8) = happyGoto action_5
action_67 (9) = happyGoto action_6
action_67 (10) = happyGoto action_7
action_67 (11) = happyGoto action_8
action_67 (12) = happyGoto action_9
action_67 _ = happyFail

action_68 (15) = happyShift action_71
action_68 _ = happyFail

action_69 (13) = happyShift action_2
action_69 (14) = happyShift action_10
action_69 (16) = happyShift action_11
action_69 (17) = happyShift action_12
action_69 (18) = happyShift action_13
action_69 (19) = happyShift action_14
action_69 (21) = happyShift action_15
action_69 (22) = happyShift action_16
action_69 (25) = happyShift action_17
action_69 (34) = happyShift action_18
action_69 (36) = happyShift action_19
action_69 (4) = happyGoto action_70
action_69 (7) = happyGoto action_4
action_69 (8) = happyGoto action_5
action_69 (9) = happyGoto action_6
action_69 (10) = happyGoto action_7
action_69 (11) = happyGoto action_8
action_69 (12) = happyGoto action_9
action_69 _ = happyFail

action_70 _ = happyReduce_3

action_71 (13) = happyShift action_2
action_71 (14) = happyShift action_10
action_71 (16) = happyShift action_11
action_71 (17) = happyShift action_12
action_71 (18) = happyShift action_13
action_71 (19) = happyShift action_14
action_71 (21) = happyShift action_15
action_71 (22) = happyShift action_16
action_71 (25) = happyShift action_17
action_71 (34) = happyShift action_18
action_71 (36) = happyShift action_19
action_71 (4) = happyGoto action_74
action_71 (7) = happyGoto action_4
action_71 (8) = happyGoto action_5
action_71 (9) = happyGoto action_6
action_71 (10) = happyGoto action_7
action_71 (11) = happyGoto action_8
action_71 (12) = happyGoto action_9
action_71 _ = happyFail

action_72 (39) = happyShift action_73
action_72 _ = happyFail

action_73 _ = happyReduce_1

action_74 _ = happyReduce_4

happyReduce_1 = happyReduce 7 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdent happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Function happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 4 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Declare happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 6 4 happyReduction_3
happyReduction_3 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (RecDeclare happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 7 4 happyReduction_4
happyReduction_4 ((HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_1  4 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn5
		 ([(happy_var_1,happy_var_3)]
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Binary Or happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  8 happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Binary And happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  9 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary EQ happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  9 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary LT happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary GT happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  9 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary LE happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary GE happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  9 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Binary Add happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  10 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Binary Sub happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  10 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  11 happyReduction_22
happyReduction_22 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Binary Mul happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  11 happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Binary Div happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  11 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happyReduce 4 12 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Call happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_1  12 happyReduction_26
happyReduction_26 (HappyTerminal (Digits happy_var_1))
	 =  HappyAbsSyn12
		 (Literal (IntV happy_var_1)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  12 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn12
		 (Literal (BoolV True)
	)

happyReduce_28 = happySpecReduce_1  12 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn12
		 (Literal (BoolV False)
	)

happyReduce_29 = happySpecReduce_2  12 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Unary Neg happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  12 happyReduction_30
happyReduction_30 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Unary Not happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  12 happyReduction_31
happyReduction_31 (HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn12
		 (Variable happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  12 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 41 41 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenKeyword "function" -> cont 13;
	TokenKeyword "if" -> cont 14;
	TokenKeyword "else" -> cont 15;
	TokenKeyword "true" -> cont 16;
	TokenKeyword "false" -> cont 17;
	TokenKeyword "var" -> cont 18;
	TokenKeyword "rec" -> cont 19;
	Symbol ";" -> cont 20;
	TokenIdent happy_dollar_dollar -> cont 21;
	Digits happy_dollar_dollar -> cont 22;
	Symbol "=" -> cont 23;
	Symbol "+" -> cont 24;
	Symbol "-" -> cont 25;
	Symbol "*" -> cont 26;
	Symbol "/" -> cont 27;
	Symbol "<" -> cont 28;
	Symbol ">" -> cont 29;
	Symbol "<=" -> cont 30;
	Symbol ">=" -> cont 31;
	Symbol "==" -> cont 32;
	Symbol "&&" -> cont 33;
	Symbol "!" -> cont 34;
	Symbol "||" -> cont 35;
	Symbol "(" -> cont 36;
	Symbol ")" -> cont 37;
	Symbol "{" -> cont 38;
	Symbol "}" -> cont 39;
	Symbol "," -> cont 40;
	_ -> happyError' (tk:tks)
	}

happyError_ 41 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . happyError

parser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


symbols = ["+", "-", "*", "/", "(", ")", "{", "}", ";", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!", ","]
keywords = ["function", "var", "rec", "if", "else", "true", "false"]
parseExp str = parser (lexer symbols keywords str)

parseInput = do
  input <- getContents
  print (parseExp input)
{-# LINE 1 "templates/GenericTemplate.hs" #-}


















-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

























infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

