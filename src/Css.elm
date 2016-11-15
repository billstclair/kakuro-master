-- This is a copy of https://github.com/massung/elm-css/Css.elm,
-- upgraded to work with Elm 0.18.
-- I'll switch back to Massung's copy, once he upgrades.

module Css exposing
    ( Stylesheet
    , Sel(..)
    , Pseudo(..)
    , Descriptor
    , Rule
    , stylesheet
    , style
    )

{-| This module is designed to allow you to add type-safe CSS styling
to your rendered Html via style tags. It has basic support for @import
directives and CSS rules.

Assuming you have types for id and class attributes, you "compile" a
Stylesheet with the stylesheet function and use the functions returned
in the Stylesheet to generate id and class attributes.

For example:

    type Id = MyId
    type Class = MyClass
    
    -- import some google fonts
    imports =
        ["https://fonts.googleapis.com/css?family=Droid+Sans:400,700"
        ]

    -- create ruels, notice the use of MyId and MyClass. 
    rules =
        [ { selectors = [ Css.Id MyId ]
          , descriptor = [ ("color", "red") ]
          }
        , { selectors = [ Css.Class MyClass ]
          , descriptor = [ ("color", "blue") ]
          }
        ]
    
    -- compile a stylesheet with imports and a single rule
    stylesheet = Css.stylesheet imports rules
    
    -- now, add the style node, and safely use your ids and classes
    html =
        div []
            [ Css.style [ Html.Attributes.scoped True ] stylesheet
            , div [ stylesheet.id MyId ] [ text "Using MyId" ]
            , div [ stylesheet.class MyClass ] [ text "Using MyClass" ]
            ]

# Types
@docs Stylesheet, Sel, Pseudo, Descriptor, Rule

# Functions
@docs stylesheet, style
-}

import Html
import Html.Attributes
import String exposing (concat, cons, join)

{-| A Stylesheet is a "compiled" Html text node, as well as functions
that allow you to safely create Html.Attributes for the id and class of
your tags. It is returned by the stylesheet function.
-}
type alias Stylesheet id cls msg =
    { node : Html.Html msg
    , id : id -> Html.Attribute msg
    , class : cls -> Html.Attribute msg
    , classes : List cls -> Html.Attribute msg
    }

{-| CSS rule selectors follow the all the selectors found [here](https://developer.mozilla.org/en-US/docs/Web/CSS/Attribute_selectors)
except for attribute selectors.

The most common selectors are the Type, Id, and Class selectors. The others
are combinator selectors or pseudo-class/element selectors. Some examples:

    -- div { ... }
    Type "div"
    
    -- #MyId { ... }
    Id MyId
    
    -- .MyClass { ... }
    Class MyClass
    
    -- div #Content { ... }
    Descendant (Id Content) (Type "div")
    
    -- span > .MyClass { ... }
    Child (Class MyClass) (Type "span")
    
    -- hr ~ p:first-line:first-letter { ... }
    Pseudo [FirstLine, FirstLetter] <| Sibling (Type "p") (Type "hr")

Take a moment to notice that for combinators, the most specific element being
styled (what would be last in the selector) appears first in code.
-}
type Sel id cls
    = Type String
    | Id id
    | Class cls
    | Descendant (Sel id cls) (Sel id cls)
    | Child (Sel id cls) (Sel id cls)
    | Sibling (Sel id cls) (Sel id cls)
    | Adjacent (Sel id cls) (Sel id cls)
    | Pseudo (List Pseudo) (Sel id cls)

{-| Pseudo CSS selectors and elements. These are chained together in the
Pseudo selector.
-}
type Pseudo
    = Any
    | Default
    | Link
    | Visited
    | Hover
    | Active
    | Focus
    | Target
    | Enabled
    | Disabled
    | Checked
    | Indeterminate
    | Invalid
    | Valid
    | Fullscreen
    | Root
    | Scope
    | FirstChild
    | LastChild
    | NthChild Int
    | NthLastChild Int
    | NthOfType String
    | NthLastOfType String
    | FirstOfType
    | LastOfType
    | OnlyOfType
    | Empty
    | Left
    | Right
    | Lang String
    | Dir String
    | FirstLetter
    | FirstLine
    | Before
    | After
    | Selection
    | Backdrop

{-| A list of key/value style properties. -}
type alias Descriptor = List (String, String)

{-| A Rule is a list of matching selectors and a descriptor, which is
a list of key/value style pairs. Each selector is a separate possible match
for the rule. For example:

    [ Sel.Id MyId
    , Sel.Class MyClass
    , Sel.Type "div"
    , Sel.Sibling (Sel.Type "a") (Sel.Type "hr")
    ]
    
That list of selectors would be the same as the following rule in CSS:

    #MyId, .MyClass, div, hr a { ... }
-}
type alias Rule id cls =
    { selectors : List (Sel id cls)
    , descriptor : Descriptor
    }

{-| Render a pseudo selector/element to a string. -}
pseudo : Pseudo -> String
pseudo p = 
    case p of
        Any -> ":any"
        Default -> ":default"
        Link -> ":link"
        Visited -> ":visited"
        Hover -> ":hover"
        Active -> ":active"
        Focus -> ":focus"
        Target -> ":target"
        Enabled -> ":enabled"
        Disabled -> ":disabled"
        Checked -> ":checked"
        Indeterminate -> ":indeterminate"
        Invalid -> ":invalid"
        Valid -> ":valid"
        Fullscreen -> ":fullscreen"
        Root -> ":root"
        Scope -> ":scope"
        FirstChild -> ":first-child"
        LastChild -> ":last-child"
        NthChild n -> ":nth-child(" ++ (toString n) ++ ")"
        NthLastChild n -> ":nth-last-child(" ++ (toString n) ++ ")"
        NthOfType s -> ":nth-of-type(" ++ s ++ ")"
        NthLastOfType s -> ":nth-last-of-type(" ++ s ++ ")"
        FirstOfType -> ":first-of-type"
        LastOfType -> ":last-of-type"
        OnlyOfType -> ":only-of-type"
        Lang s -> ":lang(" ++ s ++ ")"
        Dir s -> ":dir(" ++ s ++ ")"
        Empty -> ":empty"
        Left -> ":left"
        Right -> ":right"
        FirstLetter -> "::first-letter"
        FirstLine -> "::first-line"
        Before -> "::before"
        After -> "::after"
        Selection -> "::selection"
        Backdrop -> "::backdrop"

{-| Render a selector to a string. -}
sel : Sel id cls -> String
sel selector =
    case selector of
        Type element -> element
        Id id -> cons '#' (toString id)
        Class cls -> cons '.' (toString cls)
        Descendant s1 s2 -> join " " [sel s2, sel s1]
        Child s1 s2 -> join " > " [sel s2, sel s1]
        Sibling s1 s2 -> join " ~ " [sel s2, sel s1]
        Adjacent s1 s2 -> join " + " [sel s2, sel s1]
        Pseudo ps s -> concat <| sel s :: (List.map pseudo ps)

{-| Render a descriptor to a string. -}
desc : Descriptor -> String
desc =
    concat << List.map (\(k, v) -> concat [k, ":", v, ";"])

{-| Render a rule (selectors and descriptor) to a string. -}
rule : Rule id class -> String
rule rule =
    concat
        [ join "," <| List.map sel rule.selectors
        , "{"
        , desc rule.descriptor
        , "}" 
        ]

{-| Render a url to an @import directive. -}
importUrl : String -> String
importUrl url =
    concat [ "@import url(", url, ");" ]

{-| Compiles a Stylesheet if given a list of urls to @import and
a list of Rules to generate. The Stylesheet contains the imports,
rules (rendered as strings) and functions for creating type-safe
id and class attributes.
-} 
stylesheet : List String -> List (Rule id cls) -> Stylesheet id cls msg
stylesheet urls rules =
    { node = Html.text << concat <|
        (List.map importUrl urls) ++
        (List.map rule rules)
    , id = Html.Attributes.id << toString
    , class = Html.Attributes.class << toString
    , classes = Html.Attributes.class << join " " << List.map toString
    }

{-| Render a style tag node. -}
style : List (Html.Attribute msg) -> Stylesheet id cls msg -> Html.Html msg
style attributes sheet =
    Html.node "style" attributes [ sheet.node ]
