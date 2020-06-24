module CustomToVirtualDom exposing (toVirtualDom)

import Html exposing (Attribute, Html, text)
import Html.Parser exposing (Node(..))
import Html.Parser.Util exposing (toVirtualDom)


{-| Converts nodes to virtual dom nodes.
-}
toVirtualDom : (( String, String ) -> Attribute msg) -> List Node -> List (Html msg)
toVirtualDom toAttribute nodes =
    List.map (toVirtualDomEach toAttribute) nodes


toVirtualDomEach : (( String, String ) -> Attribute msg) -> Node -> Html msg
toVirtualDomEach toAttribute node =
    case node of
        Element name attrs children ->
            Html.node name (List.map toAttribute attrs) (toVirtualDom toAttribute children)

        Text s ->
            text s

        Comment _ ->
            text ""
