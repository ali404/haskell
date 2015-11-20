-- Informatics 1 - Functional Programming
-- Tutorial 7
--
-- Week 9 - Due: 19/20 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
-- this is not good, rewrite
split :: Command -> [Command]
split (Sit :#: cmd2) = split cmd2
split (cmd1 :#: cmd2) = [cmd1] ++ split cmd2
split cmd1 = [cmd1]
split [] = []

-- 1b. join
join :: [Command] -> Command
join (cmd1:cmds) = cmd1 :#: join cmds
join [] = Sit

equivalent :: Command -> Command -> Bool
equivalent c1 c2 = split c1 == split c2

prop_split_join :: Command -> Bool
prop_split_join cmd = equivalent (join (split cmd)) cmd

-- rewrite
prop_split :: Command -> Bool
prop_split cmd = elem Sit cmdList
  where
    cmdList = split cmd


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy 1 cmd = cmd
copy repeats cmd = cmd :#: copy (repeats-1) cmd

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon size = copy 5 (Go size :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon size turns = copy turns (Go size :#: Turn (fromIntegral (div 360 turns)))

-- Exercise 3
-- spiral
--what if side is lower than 0 ?
--rewrite!
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral side 1 step angle = Go side :#: Turn angle
spiral side n step angle
  | side + step <= 0 = Go side :#: Turn angle
  | otherwise = Go side :#: Turn angle :#: spiral (side+step) (n-1) step angle


-- Exercise 4
-- optimise
-- rewrite !
optimise :: Command -> Command
optimise (Go x :#: Go y :#: cmds) = optimise (Go (x+y) :#: cmds)
optimise (Go 0 :#: cmds) = optimise cmds
optimise (cmd :#: Sit :#: cmds) = optimise (cmd :#: cmds)
optimise (Turn x :#: Turn y :#: cmds) = optimise (Turn (x+y) :#: cmds)
optimise (Turn 0 :#: cmds) = optimise cmds
optimise (cmd :#: cmds) = cmd :#: optimise cmds
optimise cmd = cmd

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
  where
    f x
      | x == 0 = Go 10
      | otherwise = g (x-1) :#: n :#: f (x-1) :#: n :#: g (x-1)
    g x
      | x == 0 = Go 10
      | otherwise = f (x-1) :#: p :#: g (x-1) :#: p :#:  f (x-1)
    n = Turn 60
    p = Turn (-60)

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: p :#: p :#: f x :#: p :#: p :#: f x :#: p :#: p
  where
    f x
      | x == 0 = Go 10
      | otherwise = f (x-1) :#: n :#: f (x-1) :#: p :#: p  :#: f (x-1) :#: n :#: f (x-1)
    n = Turn 60
    p = Turn (-60)

-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l x
   where
     l x
       | x == 0 = Go 10
       | otherwise = n :#: r (x-1) :#: f  :#: p :#: l (x-1) :#: f  :#: l (x-1) :#: p :#: f :#: r (x-1) :#: n
     r x
       | x == 0 = Go 10
       | otherwise = p :#: l (x-1) :#: f :#: n :#: r (x-1) :#: f :#: r (x-1) :#: n :#: f :#: l (x-1) :#: p
     f = Go 10
     n = Turn 90
     p = Turn (-90)

peanoGosper :: Int -> Command
peanoGosper x = f x
  where
    f x
      | x == 0 = Go 10
      | otherwise = f (x-1) :#: p :#: g (x-1) :#: p :#: p :#: g (x-1) :#: n
                    :#: f (x-1) :#: n :#: n :#: f (x-1) :#: f (x-1) :#: n :#: g (x-1) :#: p
    g x
      | x == 0 = Go 10
      | otherwise = n :#: f (x-1) :#: p  :#: g (x-1) :#: g (x-1) :#: p :#: p :#: g (x-1) :#: p :#:
                    f (x-1) :#: n :#: n :#: f (x-1) :#: n :#: g (x-1)
    n = Turn (-60)
    p = Turn (60)

cross :: Int -> Command
cross x = copy 4 (f x :#: n)
  where
    f x
      | x == 0 = Go 10
      | otherwise = f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1)
                    :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1)
    n = Turn (-90)
    p = Turn 90

segment :: Int -> Command
segment x = copy 3 (f x :#: p) :#: f x
  where
    f x
      | x == 0 = Go 10
      | otherwise = n :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f(x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1)  :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1)  :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f(x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: p
    p = Turn (90)
    n = Turn (-90)
