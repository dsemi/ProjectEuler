module Main where

import Control.Monad
import Data.List.Split
import Data.Maybe
import Euler.Util
import System.Directory
import System.Environment (getArgs)
import System.Environment.FindBin (getProgPath)
import System.Random
import Text.Printf

import Euler.Problem001
import Euler.Problem002
import Euler.Problem003
import Euler.Problem004
import Euler.Problem005
import Euler.Problem006
import Euler.Problem007
import Euler.Problem008
import Euler.Problem009
import Euler.Problem010

import Euler.Problem011
import Euler.Problem012
import Euler.Problem013
import Euler.Problem014
import Euler.Problem015
import Euler.Problem016
import Euler.Problem017
import Euler.Problem018
import Euler.Problem019
import Euler.Problem020

import Euler.Problem021
import Euler.Problem022
import Euler.Problem023
import Euler.Problem024
import Euler.Problem025
import Euler.Problem026
import Euler.Problem027
import Euler.Problem028
import Euler.Problem029
import Euler.Problem030

import Euler.Problem031
import Euler.Problem032
import Euler.Problem033
import Euler.Problem034
import Euler.Problem035
import Euler.Problem036
import Euler.Problem037
import Euler.Problem038
import Euler.Problem039
import Euler.Problem040

import Euler.Problem041
import Euler.Problem042
import Euler.Problem043
import Euler.Problem044
import Euler.Problem045
import Euler.Problem046
import Euler.Problem047
import Euler.Problem048
import Euler.Problem049
import Euler.Problem050

import Euler.Problem051
import Euler.Problem052
import Euler.Problem053
import Euler.Problem054
import Euler.Problem055
import Euler.Problem056
import Euler.Problem057
import Euler.Problem058
import Euler.Problem059
import Euler.Problem060

import Euler.Problem061
import Euler.Problem062
import Euler.Problem063
import Euler.Problem064
import Euler.Problem065
import Euler.Problem066
import Euler.Problem067
import Euler.Problem068
import Euler.Problem069
import Euler.Problem070

import Euler.Problem071
import Euler.Problem072
import Euler.Problem073
import Euler.Problem074
import Euler.Problem075
import Euler.Problem076
import Euler.Problem077
import Euler.Problem078
import Euler.Problem079
import Euler.Problem080

import Euler.Problem081
import Euler.Problem082
import Euler.Problem083
import Euler.Problem084
import Euler.Problem085
import Euler.Problem086
import Euler.Problem087
import Euler.Problem088
import Euler.Problem089
import Euler.Problem090

import Euler.Problem091
import Euler.Problem092
import Euler.Problem093
import Euler.Problem094
import Euler.Problem095
import Euler.Problem096
import Euler.Problem097
import Euler.Problem098
import Euler.Problem099
import Euler.Problem100

import Euler.Problem101
import Euler.Problem102
-- import Euler.Problem103
import Euler.Problem104
-- import Euler.Problem105
-- import Euler.Problem106
import Euler.Problem107
import Euler.Problem108
-- import Euler.Problem109
-- import Euler.Problem110

-- import Euler.Problem111
import Euler.Problem112
import Euler.Problem113
import Euler.Problem114
import Euler.Problem115
import Euler.Problem116
import Euler.Problem117
import Euler.Problem118
import Euler.Problem119
import Euler.Problem120

import Euler.Problem121
import Euler.Problem122
import Euler.Problem123
import Euler.Problem124
import Euler.Problem125
-- import Euler.Problem126
import Euler.Problem127
-- import Euler.Problem128
import Euler.Problem129
import Euler.Problem130

-- import Euler.Problem131
import Euler.Problem132
import Euler.Problem133
import Euler.Problem134
-- import Euler.Problem135
-- import Euler.Problem136
-- import Euler.Problem137
-- import Euler.Problem138
import Euler.Problem139
-- import Euler.Problem140

-- import Euler.Problem141
import Euler.Problem142
-- import Euler.Problem143
import Euler.Problem144
import Euler.Problem145
-- import Euler.Problem146
-- import Euler.Problem147
import Euler.Problem148
-- import Euler.Problem149
-- import Euler.Problem150

-- import Euler.Problem151
import Euler.Problem152
-- import Euler.Problem153
-- import Euler.Problem154
-- import Euler.Problem155
-- import Euler.Problem156
-- import Euler.Problem157
-- import Euler.Problem158
-- import Euler.Problem159
import Euler.Problem160

-- import Euler.Problem161
import Euler.Problem162
-- import Euler.Problem163
import Euler.Problem164
-- import Euler.Problem165
-- import Euler.Problem166
-- import Euler.Problem167
-- import Euler.Problem168
-- import Euler.Problem169
-- import Euler.Problem170

-- import Euler.Problem171
-- import Euler.Problem172
-- import Euler.Problem173
-- import Euler.Problem174
-- import Euler.Problem175
-- import Euler.Problem176
-- import Euler.Problem177
import Euler.Problem178
-- import Euler.Problem179
-- import Euler.Problem180

import Euler.Problem181
-- import Euler.Problem182
-- import Euler.Problem183
-- import Euler.Problem184
import Euler.Problem185
-- import Euler.Problem186
import Euler.Problem187
import Euler.Problem188
-- import Euler.Problem189
-- import Euler.Problem190

import Euler.Problem191
-- import Euler.Problem192
-- import Euler.Problem193
-- import Euler.Problem194
-- import Euler.Problem195
-- import Euler.Problem196
-- import Euler.Problem197
-- import Euler.Problem198
-- import Euler.Problem199
-- import Euler.Problem200

-- import Euler.Problem201
-- import Euler.Problem202
-- import Euler.Problem203
import Euler.Problem204
-- import Euler.Problem205
import Euler.Problem206
import Euler.Problem207
-- import Euler.Problem208
-- import Euler.Problem209
-- import Euler.Problem210

-- import Euler.Problem211
-- import Euler.Problem212
-- import Euler.Problem213
-- import Euler.Problem214
-- import Euler.Problem215
-- import Euler.Problem216
-- import Euler.Problem217
-- import Euler.Problem218
-- import Euler.Problem219
-- import Euler.Problem220

-- import Euler.Problem221
-- import Euler.Problem222
-- import Euler.Problem223
-- import Euler.Problem224
-- import Euler.Problem225
-- import Euler.Problem226
-- import Euler.Problem227
-- import Euler.Problem228
-- import Euler.Problem229
-- import Euler.Problem230

-- import Euler.Problem231
-- import Euler.Problem232
-- import Euler.Problem233
-- import Euler.Problem234
-- import Euler.Problem235
-- import Euler.Problem236
-- import Euler.Problem237
-- import Euler.Problem238
-- import Euler.Problem239
-- import Euler.Problem240

-- import Euler.Problem241
-- import Euler.Problem242
import Euler.Problem243
-- import Euler.Problem244
-- import Euler.Problem245
-- import Euler.Problem246
-- import Euler.Problem247
-- import Euler.Problem248
-- import Euler.Problem249
-- import Euler.Problem250

-- import Euler.Problem251
-- import Euler.Problem252
-- import Euler.Problem253
-- import Euler.Problem254
-- import Euler.Problem255
-- import Euler.Problem256
-- import Euler.Problem257
-- import Euler.Problem258
-- import Euler.Problem259
-- import Euler.Problem260

-- import Euler.Problem261
-- import Euler.Problem262
-- import Euler.Problem263
-- import Euler.Problem264
-- import Euler.Problem265
-- import Euler.Problem266
import Euler.Problem267
-- import Euler.Problem268
-- import Euler.Problem269
-- import Euler.Problem270

-- import Euler.Problem271
-- import Euler.Problem272
-- import Euler.Problem273
-- import Euler.Problem274
-- import Euler.Problem275
-- import Euler.Problem276
import Euler.Problem277
-- import Euler.Problem278
-- import Euler.Problem279
-- import Euler.Problem280

-- import Euler.Problem281
-- import Euler.Problem282
-- import Euler.Problem283
-- import Euler.Problem284
-- import Euler.Problem285
-- import Euler.Problem286
-- import Euler.Problem287
-- import Euler.Problem288
-- import Euler.Problem289
-- import Euler.Problem290

-- import Euler.Problem291
-- import Euler.Problem292
-- import Euler.Problem293
-- import Euler.Problem294
-- import Euler.Problem295
-- import Euler.Problem296
-- import Euler.Problem297
-- import Euler.Problem298
-- import Euler.Problem299
-- import Euler.Problem300

-- import Euler.Problem301
-- import Euler.Problem302
-- import Euler.Problem303
-- import Euler.Problem304
-- import Euler.Problem305
-- import Euler.Problem306
-- import Euler.Problem307
-- import Euler.Problem308
-- import Euler.Problem309
-- import Euler.Problem310

-- import Euler.Problem311
-- import Euler.Problem312
-- import Euler.Problem313
-- import Euler.Problem314
-- import Euler.Problem315
-- import Euler.Problem316
-- import Euler.Problem317
-- import Euler.Problem318
-- import Euler.Problem319
-- import Euler.Problem320

-- import Euler.Problem321
-- import Euler.Problem322
-- import Euler.Problem323
-- import Euler.Problem324
-- import Euler.Problem325
-- import Euler.Problem326
-- import Euler.Problem327
-- import Euler.Problem328
-- import Euler.Problem329
-- import Euler.Problem330

-- import Euler.Problem331
-- import Euler.Problem332
-- import Euler.Problem333
-- import Euler.Problem334
-- import Euler.Problem335
-- import Euler.Problem336
-- import Euler.Problem337
-- import Euler.Problem338
-- import Euler.Problem339
-- import Euler.Problem340

-- import Euler.Problem341
-- import Euler.Problem342
-- import Euler.Problem343
-- import Euler.Problem344
-- import Euler.Problem345
-- import Euler.Problem346
-- import Euler.Problem347
-- import Euler.Problem348
-- import Euler.Problem349
-- import Euler.Problem350

-- import Euler.Problem351
-- import Euler.Problem352
-- import Euler.Problem353
-- import Euler.Problem354
-- import Euler.Problem355
-- import Euler.Problem356
-- import Euler.Problem357
-- import Euler.Problem358
-- import Euler.Problem359
-- import Euler.Problem360

-- import Euler.Problem361
-- import Euler.Problem362
-- import Euler.Problem363
-- import Euler.Problem364
-- import Euler.Problem365
-- import Euler.Problem366
-- import Euler.Problem367
-- import Euler.Problem368
-- import Euler.Problem369
-- import Euler.Problem370

-- import Euler.Problem371
-- import Euler.Problem372
-- import Euler.Problem373
-- import Euler.Problem374
-- import Euler.Problem375
-- import Euler.Problem376
-- import Euler.Problem377
-- import Euler.Problem378
-- import Euler.Problem379
-- import Euler.Problem380

-- import Euler.Problem381
-- import Euler.Problem382
-- import Euler.Problem383
-- import Euler.Problem384
-- import Euler.Problem385
-- import Euler.Problem386
-- import Euler.Problem387
-- import Euler.Problem388
-- import Euler.Problem389
-- import Euler.Problem390

-- import Euler.Problem391
-- import Euler.Problem392
-- import Euler.Problem393
-- import Euler.Problem394
-- import Euler.Problem395
-- import Euler.Problem396
-- import Euler.Problem397
-- import Euler.Problem398
-- import Euler.Problem399
-- import Euler.Problem400

-- import Euler.Problem401
-- import Euler.Problem402
-- import Euler.Problem403
-- import Euler.Problem404
-- import Euler.Problem405
-- import Euler.Problem406
-- import Euler.Problem407
-- import Euler.Problem408
-- import Euler.Problem409
-- import Euler.Problem410

-- import Euler.Problem411
-- import Euler.Problem412
-- import Euler.Problem413
-- import Euler.Problem414
-- import Euler.Problem415
-- import Euler.Problem416
-- import Euler.Problem417
-- import Euler.Problem418
-- import Euler.Problem419
-- import Euler.Problem420

-- import Euler.Problem421
-- import Euler.Problem422
-- import Euler.Problem423
-- import Euler.Problem424
-- import Euler.Problem425
-- import Euler.Problem426
-- import Euler.Problem427
-- import Euler.Problem428
-- import Euler.Problem429
-- import Euler.Problem430

-- import Euler.Problem431
-- import Euler.Problem432
-- import Euler.Problem433
-- import Euler.Problem434
-- import Euler.Problem435
-- import Euler.Problem436
-- import Euler.Problem437
-- import Euler.Problem438
-- import Euler.Problem439
-- import Euler.Problem440

-- import Euler.Problem441
-- import Euler.Problem442
import Euler.Problem443
-- import Euler.Problem444
-- import Euler.Problem445
-- import Euler.Problem446
-- import Euler.Problem447
-- import Euler.Problem448
-- import Euler.Problem449
-- import Euler.Problem450

-- import Euler.Problem451
-- import Euler.Problem452
-- import Euler.Problem453
-- import Euler.Problem454
-- import Euler.Problem455
-- import Euler.Problem456
-- import Euler.Problem457
-- import Euler.Problem458
-- import Euler.Problem459
-- import Euler.Problem460

-- import Euler.Problem461
-- import Euler.Problem462
-- import Euler.Problem463
-- import Euler.Problem464
-- import Euler.Problem465
-- import Euler.Problem466
-- import Euler.Problem467
-- import Euler.Problem468
-- import Euler.Problem469
-- import Euler.Problem470

-- import Euler.Problem471
-- import Euler.Problem472
-- import Euler.Problem473
-- import Euler.Problem474
-- import Euler.Problem475
-- import Euler.Problem476
-- import Euler.Problem477
-- import Euler.Problem478
-- import Euler.Problem479
-- import Euler.Problem480

-- import Euler.Problem481
-- import Euler.Problem482
-- import Euler.Problem483
-- import Euler.Problem484
-- import Euler.Problem485
-- import Euler.Problem486
-- import Euler.Problem487
-- import Euler.Problem488
-- import Euler.Problem489
-- import Euler.Problem490

-- import Euler.Problem491
-- import Euler.Problem492
-- import Euler.Problem493
-- import Euler.Problem494
-- import Euler.Problem495
-- import Euler.Problem496
-- import Euler.Problem497
-- import Euler.Problem498
-- import Euler.Problem499
-- import Euler.Problem500

problems = [ (   1,   problem1 )
           , (   2,   problem2 )
           , (   3,   problem3 )
           , (   4,   problem4 )
           , (   5,   problem5 )
           , (   6,   problem6 )
           , (   7,   problem7 )
           , (   8,   problem8 )
           , (   9,   problem9 )
           , (  10,  problem10 )
           , (  11,  problem11 )
           , (  12,  problem12 )
           , (  13,  problem13 )
           , (  14,  problem14 )
           , (  15,  problem15 )
           , (  16,  problem16 )
           , (  17,  problem17 )
           , (  18,  problem18 )
           , (  19,  problem19 )
           , (  20,  problem20 )
           , (  21,  problem21 )
           , (  22,  problem22 )
           , (  23,  problem23 )
           , (  24,  problem24 )
           , (  25,  problem25 )
           , (  26,  problem26 )
           , (  27,  problem27 )
           , (  28,  problem28 )
           , (  29,  problem29 )
           , (  30,  problem30 )
           , (  31,  problem31 )
           , (  32,  problem32 )
           , (  33,  problem33 )
           , (  34,  problem34 )
           , (  35,  problem35 )
           , (  36,  problem36 )
           , (  37,  problem37 )
           , (  38,  problem38 )
           , (  39,  problem39 )
           , (  40,  problem40 )
           , (  41,  problem41 )
           , (  42,  problem42 )
           , (  43,  problem43 )
           , (  44,  problem44 )
           , (  45,  problem45 )
           , (  46,  problem46 )
           , (  47,  problem47 )
           , (  48,  problem48 )
           , (  49,  problem49 )
           , (  50,  problem50 )
           , (  51,  problem51 )
           , (  52,  problem52 )
           , (  53,  problem53 )
           , (  54,  problem54 )
           , (  55,  problem55 )
           , (  56,  problem56 )
           , (  57,  problem57 )
           , (  58,  problem58 )
           , (  59,  problem59 )
           , (  60,  problem60 )
           , (  61,  problem61 )
           , (  62,  problem62 )
           , (  63,  problem63 )
           , (  64,  problem64 )
           , (  65,  problem65 )
           , (  66,  problem66 )
           , (  67,  problem67 )
           , (  68,  problem68 )
           , (  69,  problem69 )
           , (  70,  problem70 )
           , (  71,  problem71 )
           , (  72,  problem72 )
           , (  73,  problem73 )
           , (  74,  problem74 )
           , (  75,  problem75 )
           , (  76,  problem76 )
           , (  77,  problem77 )
           , (  78,  problem78 )
           , (  79,  problem79 )
           , (  80,  problem80 )
           , (  81,  problem81 )
           , (  82,  problem82 )
           , (  83,  problem83 )
           , (  84,  problem84 )
           , (  85,  problem85 )
           , (  86,  problem86 )
           , (  87,  problem87 )
           , (  88,  problem88 )
           , (  89,  problem89 )
           , (  90,  problem90 )
           , (  91,  problem91 )
           , (  92,  problem92 )
           , (  93,  problem93 )
           , (  94,  problem94 )
           , (  95,  problem95 )
           , (  96,  problem96 )
           , (  97,  problem97 )
           , (  98,  problem98 )
           , (  99,  problem99 )
           , ( 100, problem100 )
           , ( 101, problem101 )
           , ( 102, problem102 )
           , ( 104, problem104 )
           , ( 107, problem107 )
           , ( 108, problem108 )
           , ( 112, problem112 )
           , ( 113, problem113 )
           , ( 114, problem114 )
           , ( 115, problem115 )
           , ( 116, problem116 )
           , ( 117, problem117 )
           , ( 118, problem118 )
           , ( 119, problem119 )
           , ( 120, problem120 )
           , ( 121, problem121 )
           , ( 122, problem122 )
           , ( 123, problem123 )
           , ( 124, problem124 )
           , ( 125, problem125 )
           , ( 127, problem127 )
           , ( 129, problem129 )
           , ( 130, problem130 )
           , ( 132, problem132 )
           , ( 133, problem133 )
           , ( 134, problem134 )
           , ( 139, problem139 )
           , ( 142, problem142 )
           , ( 144, problem144 )
           , ( 145, problem145 )
           , ( 148, problem148 )
           , ( 152, problem152 )
           , ( 160, problem160 )
           , ( 162, problem162 )
           , ( 164, problem164 )
           , ( 178, problem178 )
           , ( 181, problem181 )
           , ( 185, problem185 )
           , ( 187, problem187 )
           , ( 188, problem188 )
           , ( 191, problem191 )
           , ( 204, problem204 )
           , ( 206, problem206 )
           , ( 207, problem207 )
           , ( 243, problem243 )
           , ( 267, problem267 )
           , ( 277, problem277 )
           , ( 443, problem443 )
           ]

data Arguments = Args { missing  :: Bool
                      , analyze  :: Bool
                      , help     :: Bool
                      , noans    :: Bool
                      , probs :: [Int]
                      } deriving (Show)

addMissing   (Args _ b c d e) = Args True b c d e
addAnalyze   (Args a _ c d e) = Args a True c d e
addHelp      (Args a b _ d e) = Args a b True d e
addNoans     (Args a b c _ e) = Args a b c True e
addProblem n (Args a b c d e) = Args a b c d $ n:e

parseArgs args = let (Args a b c d e) = foldr pa (Args False False False False []) args
                 in Args a b c d $ if null e then [1..500] else e
    where pa "missing" m = addMissing m
          pa "analyze" m = addAnalyze m
          pa a m
              | a == "-h" || a == "--help"    = addHelp m
              | a == "-n" || a == "--noans"   = addNoans m
              | all (`elem` '-':['0'..'9']) a = case map read (splitOn "-" a) of
                                                  [s,e] -> foldr addProblem m [s..e]
                                                  [n]   -> addProblem n m
                                                  _     -> undefined -- lazy
              | otherwise                     = undefined -- again

findInput :: Int -> IO String
findInput n = do
  inputFile <- liftM (head . filter (\x -> x /= ".." && x /= ".")) $ getDirectoryContents dir
  readFile $ dir ++ inputFile
      where dir = "src/Euler/" ++ show n ++ "/"

run :: (Integral a) => Int -> Problem a -> IO ()
run n p = case p of
            (NoInputS prob)     -> printf str n prob
            (NoInputI prob)     -> printf int n $ toInteger prob
            (HasInputS prob)    -> findInput n >>= printf str n . prob
            (HasInputI prob)    -> findInput n >>= printf int n . toInteger . prob
            (HasRandoS ab prob) -> newStdGen >>= printf str n . prob . randomRs ab
    where str = "Problem %3d: %28s\n"
          int = "Problem %3d: %28d\n"

main = do
  basedir <- getProgPath -- Need to chdir to here
  args <- liftM parseArgs getArgs
  forM (probs args) $ \n -> maybe (printf "Problem %d is not implemented\n" n) (run n) $ lookup n problems
