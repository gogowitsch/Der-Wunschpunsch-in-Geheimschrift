module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Random
import Char
import String
import List
import Array


main =
    App.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { echterSatz : String
    , buchstabenListe :
        List BuchstabenEintrag
        --, anzahlDerFehler : Int
    }


type alias BuchstabenEintrag =
    { geheimerBuchstabe : Char
    , zufälligerBuchstabe : Char
    }


model : Model
model =
    Model ("Hallo Welt!") (baueDieBuchstabenTabelle [] 65 (65 + 26))


baueDieBuchstabenTabelle : List BuchstabenEintrag -> Int -> Int -> List BuchstabenEintrag
baueDieBuchstabenTabelle buchstabenTabelle aktuellerBuchstabe letzterBuchstabe =
    if aktuellerBuchstabe > letzterBuchstabe then
        buchstabenTabelle
    else
        baueDieBuchstabenTabelle
            (buchstabenTabelle
                ++ [ { geheimerBuchstabe = Char.fromCode (aktuellerBuchstabe)
                     , zufälligerBuchstabe = 'H'
                     }
                   ]
            )
            (aktuellerBuchstabe + 1)
            letzterBuchstabe


verschluessleDenBuchstaben : String -> Int -> Maybe String
verschluessleDenBuchstaben echterBuchstabe indexInDerTabelle =
    if indexInDerTabelle >= List.length model.buchstabenListe then
        Nothing
    else
        let
            liste =
                String.toList echterBuchstabe

            c =
                List.head liste

            buchstabenArray =
                Array.fromList model.buchstabenListe

            tabellenEintrag : Maybe BuchstabenEintrag
            tabellenEintrag =
                Array.get indexInDerTabelle buchstabenArray
        in
            case c of
                Nothing ->
                    Nothing

                Just c2 ->
                    case tabellenEintrag of
                        Nothing ->
                            Nothing

                        Just echterTabellenEintrag ->
                            if echterTabellenEintrag.geheimerBuchstabe == c2 then
                                Just (String.fromChar echterTabellenEintrag.zufälligerBuchstabe)
                            else
                                verschluessleDenBuchstaben echterBuchstabe (indexInDerTabelle + 1)



-- UPDATE


type Msg
    = BuchstabeGeraten Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        BuchstabeGeraten derWievielteBuchstabe geratenerBuchstabe ->
            --{ model | anzahlDerFehler = 1 }
            model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.h1 [] [ text "Der Wunschpunsch in Geheimschrift" ]
        , div []
            (zeigeDieBuchstaben
                model
                0
                [ span [] [] ]
            )
        , Html.hr [] []
        , text (toString model.buchstabenListe)
        ]


zeigeDieBuchstaben : Model -> Int -> List (Html.Html Msg) -> List (Html.Html Msg)
zeigeDieBuchstaben model aktuellerBuchstabe derDom =
    if aktuellerBuchstabe >= (String.length model.echterSatz) then
        derDom
    else
        let
            buchstabe : String
            buchstabe =
                String.slice aktuellerBuchstabe (aktuellerBuchstabe + 1) model.echterSatz
        in
            (zeigeDieBuchstaben model
                (aktuellerBuchstabe + 1)
                (derDom
                    ++ [ span
                            [ style
                                [ ( "display", "inline-block" )
                                , ( "min-width", "1em" )
                                , ( "text-align", "center" )
                                , ( "vertical-align", "top" )
                                ]
                            ]
                            (buchstabenInput model aktuellerBuchstabe buchstabe)
                       ]
                )
            )


buchstabenInput : Model -> Int -> String -> List (Html.Html Msg)
buchstabenInput model aktuellerBuchstabenIndex echterBuchstabe =
    let
        verschlüsselungsergebnis =
            verschluessleDenBuchstaben (String.toUpper echterBuchstabe) 0
    in
        case verschlüsselungsergebnis of
            Nothing ->
                -- war nicht verschlüsselt
                [ text echterBuchstabe ]

            Just zufälligerBuchstabe ->
                [ text zufälligerBuchstabe
                , Html.br [] []
                , Html.input
                    [ placeholder "?"
                    , style
                        [ ( "width", "1em" )
                        , ( "text-align", "center" )
                        ]
                    , onInput (BuchstabeGeraten aktuellerBuchstabenIndex)
                    ]
                    []
                ]



{- zeigeAnzahlDerFehler : Model -> Html.Html Msg
   zeigeAnzahlDerFehler model =
       let
           message =
               if model.anzahlDerFehler > 0 then
                   "Du hast schon " ++ (toString model.anzahlDerFehler) ++ "x falsch geraten."
               else
                   ""
       in
           div [ style [ ( "color", "red" ) ] ] [ text message ]
-}
