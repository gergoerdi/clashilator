# Clashilator: Automated Clash - Verilator integration

This package provides Cabal `Setup.hs` functionality to automatically
integrate Verilator into your Clash project.

* Detailed introduction: <https://unsafePerform.IO/blog/2020-05-07-integrating_verilator_and_clash_via_cabal/>
* Example project: <https://github.com/gergoerdi/clashilator-example>

## Usage

Suppose you have a Clash circuit that you want to simulate using
Verilator, and then write Haskell code to interact with that
simulation. If your Clash code looks like this:

```
topEntity
    :: "CLK" ::: Clock System
    -> "FOO" ::: Signal System Bit
    -> "BAR" ::: Signal System (Unsigned 4)
    -> ( "BAZ"  ::: Signal System (Unsigned 10)
       , "QUUX" ::: Signal System Bit
       )
topEntity = ...
makeTopEntity 'topEntity
```

and you put this in your Cabal file (`x-clashilator-clock` can be
omitted if you have only a single clock):

```
custom-setup
  setup-depends: clashilator

executable MySim
  main-is: simulator.hs
  x-clashilator-clock: CLK
  x-clashilator-top-is: MyCircuit
```

then in your `simulator.hs`, you can import the "virtual" module
`Clash.Clashilator.FFI` which provides the following definitions:

```
data INPUT = INPUT
    { iFOO :: Bit
    , iBAR :: Word8
    }
    deriving (Show)
instance Storable INPUT

data OUTPUT = OUTPUT
    { oBAZ :: Word16
    , oQUUX :: Bit
    }
    deriving (Show)
instance Storable OUTPUT

data Sim

simInit     :: IO (Ptr Sim)
simShutdown :: Ptr Sim -> IO ()
simStep     :: Ptr Sim -> Ptr INPUT -> Ptr OUTPUT -> IO ()
```

Note that input and output buses are represented as the smallest
possible `Word` type, to improve marshalling cost when crossing the
Haskell-C++ barrier.
