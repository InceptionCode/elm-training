module Main exposing (..)

import Html exposing (Html, text)
import List exposing (map)


type alias Carts =
    { name : String
    , qty : Int
    , freeQty : Int
    }


cart : List Carts
cart =
    [ { name = "Lemon", qty = 1, freeQty = 0 }
    , { name = "Apple", qty = 5, freeQty = 0 }
    , { name = "Pear", qty = 10, freeQty = 0 }
    ]


applyFreeQty : Int -> Int -> Carts -> Carts
applyFreeQty minQty freeQty cart =
    if cart.qty == minQty then
        { cart | freeQty = freeQty }
    else
        cart


main : Html msg
main =
    map ((applyFreeQty 5 1) >> (applyFreeQty 10 3)) cart
        |> toString
        |> text
