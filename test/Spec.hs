import Test.Hspec
import Control.Exception (evaluate)
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day12

main :: IO ()
main = hspec $ 
  describe "Advent of Code 2016" $ do
    
    describe "Day01" $ do
      it "Part 1 - Test 1" $ 
        Day01.solve1 "R2, L3" `shouldBe` 5
      it "Part 1 - Test 2" $ 
        Day01.solve1 "R2, R2, R2" `shouldBe` 2
      it "Part 1 - Test 3" $ 
        Day01.solve1 "R5, L5, R5, R3" `shouldBe` 12
      it "Part 2 - Test 1" $ 
        Day01.solve2 "R8, R4, R4, R8" `shouldBe` 4
      it "Part 1 - Solution" $ do
        input <- readFile "input/day01"
        Day01.solve1 input `shouldBe` 250
      it "Part 2 - Solution" $ do
        input <- readFile "input/day01"
        Day01.solve2 input `shouldBe` 151
        
    describe "Day02" $ do
      let testInput1 = "ULL\nRRDDD\nLURDL\nUUUUD"
      it "Part 1 - Test" $ 
        Day02.solve1 testInput1 `shouldBe` "1985"
      it "Part 2 - Test" $ 
        Day02.solve2 testInput1 `shouldBe` "5DB3"
      it "Part 1 - Solution" $ do
        input <- readFile "input/day02"
        Day02.solve1 input `shouldBe` "78293"
      it "Part 2 - Solution" $ do
        input <- readFile "input/day02"
        Day02.solve2 input `shouldBe` "AC8C8"
        
    describe "Day03" $ do
      let testInput = "101 301 501\n102 302 502\n103 303 503\n201 401 601\n202 402 602\n203 403 603"
      it "Part 1 - Test" $
        Day03.solve1 "5 10 25" `shouldBe` 0
      it "Part 2 - Test" $
        Day03.solve2 testInput `shouldBe` 6
      it "Part 1 - Solution" $ do
        input <- readFile "input/day03"
        Day03.solve1 input `shouldBe` 869
      it "Part 2 - Solution" $ do
        input <- readFile "input/day03"
        Day03.solve2 input `shouldBe` 1544
        
    describe "Day04" $ do
      let testInput = "aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]\nnot-a-real-room-404[oarel]\ntotally-real-room-200[decoy]"
      it "Part 1 - Test" $
        Day04.solve1 testInput `shouldBe` 1514
      it "Part 1 - Solution" $ do
        input <- readFile "input/day04"
        Day04.solve1 input `shouldBe` 137896
      it "Part 2 - Solution" $ do
        input <- readFile "input/day04"
        Day04.solve2 input `shouldBe` 501
    
    describe "Day05" $ do
      it "Part 1 - Test" $
        Day05.solve1 "abc" `shouldBe` "18f47a30"
      it "Part 2 - Test" $
        Day05.solve2 "abc" `shouldBe` "05ace8e3"
      it "Part 1 - Solution" $
        Day05.solve1 "wtnhxymk" `shouldBe` "2414bc77"
      it "Part 2 - Solution" $
        Day05.solve2 "wtnhxymk" `shouldBe` "437e60fc"
    
    describe "Day06" $ do
      let testInput = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"
      it "Part 1 - Test" $
        Day06.solve1 testInput `shouldBe` "easter"
      it "Part 2 - Test" $
        Day06.solve2 testInput `shouldBe` "advent"
      it "Part 1 - Solution" $ do
        input <- readFile "input/day06"
        Day06.solve1 input `shouldBe` "umcvzsmw"
      it "Part 2 - Solution" $ do
        input <- readFile "input/day06"
        Day06.solve2 input `shouldBe` "rwqoacfz"
    
    describe "Day07" $ do
      it "Part 1 - Test 1" $
        Day07.solve1 "abba[mnop]qrst" `shouldBe` 1
      it "Part 1 - Test 2" $
        Day07.solve1 "abcd[bddb]xyyx" `shouldBe` 0
      it "Part 1 - Test 3" $
        Day07.solve1 "aaaa[qwer]tyui" `shouldBe` 0
      it "Part 1 - Test 4" $
        Day07.solve1 "ioxxoj[asdfgh]zxcvbn" `shouldBe` 1
      it "Part 2 - Test 1" $
        Day07.solve2 "aba[bab]xyz" `shouldBe` 1
      it "Part 2 - Test 2" $
        Day07.solve2 "xyx[xyx]xyx" `shouldBe` 0
      it "Part 2 - Test 3" $
        Day07.solve2 "aaa[kek]eke" `shouldBe` 1
      it "Part 2 - Test 4" $
        Day07.solve2 "zazbz[bzb]cdb" `shouldBe` 1
      it "Part 1 - Solution" $ do
        input <- readFile "input/day07"
        Day07.solve1 input `shouldBe` 118
      it "Part 2 - Solution" $ do
        input <- readFile "input/day07"
        Day07.solve2 input `shouldBe` 260
    
    describe "Day08" $ do
      it "Part 1 - Test" $
        Day08.process (monitor 7 3) "rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1" `shouldBe` [" #  # #", "# #    ", " #     "]
      it "Part 1 - Solution" $ do
        input <- readFile "input/day08"
        Day08.solve1 input `shouldBe` 121
      it "Part 2 - Solution" $ do
        input <- readFile "input/day08"
        Day08.solve2 input `shouldBe` 
          "###  #  # ###  #  #  ##  ####  ##  ####  ### #    \n" ++ 
          "#  # #  # #  # #  # #  # #    #  # #      #  #    \n" ++ 
          "#  # #  # #  # #  # #    ###  #  # ###    #  #    \n" ++
          "###  #  # ###  #  # #    #    #  # #      #  #    \n" ++ 
          "# #  #  # # #  #  # #  # #    #  # #      #  #    \n" ++
          "#  #  ##  #  #  ##   ##  ####  ##  ####  ### #### \n"
    
    describe "Day09" $ do
      it "Decompress - Test 1" $
        Day09.decompress "ADVENT" == "ADVENT"
      it "Decompress - Test 2" $
        Day09.decompress "A(1x5)BC" == "ABBBBBC"
      it "Decompress - Test 3" $
        Day09.decompress "(3x3)XYZ" == "XYZXYZXYZ"
      it "Decompress - Test 4" $
        Day09.decompress "A(2x2)BCD(2x2)EFG" == "ABCBCDEFEFG"
      it "Decompress - Test 5" $
        Day09.decompress "(6x1)(1x3)A" == "(1x3)A"
      it "Decompress - Test 6" $
        Day09.decompress "X(8x2)(3x3)ABCY" == "X(3x3)ABC(3x3)ABCY"
      it "Part 1 - Solution" $ do
        input <- readFile "input/day09"
        Day09.solve1 input `shouldBe` 112830
      it "Part 2 - Solution" $ do
        input <- readFile "input/day09"
        Day09.solve2 input `shouldBe` 10931789799
    
    describe "Day10" $ do
      let testInput = "value 5 goes to bot 2\n" ++
                      "bot 2 gives low to bot 1 and high to bot 0\n" ++
                      "value 3 goes to bot 1\n" ++
                      "bot 1 gives low to output 1 and high to bot 0\n" ++
                      "bot 0 gives low to output 2 and high to output 0\n" ++
                      "value 2 goes to bot 2"

      it "Part 1 - Test" $
        Day10.solve1 [5, 2] testInput `shouldBe` "bot2"
      it "Part 1 - Solution" $ do
        input <- readFile "input/day10"
        Day10.solve1 [17, 61] input `shouldBe` "bot101"
      it "Part 2 - Solution" $ do
        input <- readFile "input/day10"
        Day10.solve2 input `shouldBe` 37789
        
    describe "Day12" $ do
      let testInput = "cpy 41 a\n" ++
                      "inc a\n" ++
                      "inc a\n" ++
                      "dec a\n" ++
                      "jnz a 2\n" ++
                      "dec a\n"
      
      it "Part 1 - Test" $
        Day12.solve1 testInput `shouldBe` 42
      it "Part 1 - Solution" $ do
        input <- readFile "input/day12"
        Day12.solve1 input `shouldBe` 318009
      it "Part 2 - Solution" $ do
        input <- readFile "input/day12"
        Day12.solve2 input `shouldBe` 9227663