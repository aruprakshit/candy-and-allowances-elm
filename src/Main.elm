module Main exposing (main)

import Browser
import Parent


main : Program () Parent.Model Parent.Msg
main =
    Browser.sandbox
        { init = Parent.initialModel
        , view = Parent.view
        , update = Parent.update
        }
