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
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { echterSatz : String
    , buchstabenListe :
        List BuchstabenEintrag
    , anzahlDerFehler : Int
    , erraten : List String
    , bereitsVorhandeneVerschlüsselungen : List Int
    }


type alias BuchstabenEintrag =
    { geheimerBuchstabe : Char
    , zufälligerBuchstabe : Char
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        "Hallo Welt!"
        (baueDieBuchstabenTabelle [] 65 (65 + 26))
        0
        []
        []
    , Cmd.none
    )


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


type Verschlüsselungsergebnis
    = NichtVerschlüsselt
    | Verschlüsselt String
    | Entschlüsselt String


verschluessleDenBuchstaben : Model -> String -> Int -> Verschlüsselungsergebnis
verschluessleDenBuchstaben model echterBuchstabe indexInDerTabelle =
    if indexInDerTabelle >= List.length model.buchstabenListe then
        NichtVerschlüsselt
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
                    NichtVerschlüsselt

                Just c2 ->
                    case tabellenEintrag of
                        Nothing ->
                            NichtVerschlüsselt

                        Just echterTabellenEintrag ->
                            if echterTabellenEintrag.geheimerBuchstabe == c2 then
                                let
                                    zufälligerBuchstabe =
                                        (String.fromChar echterTabellenEintrag.zufälligerBuchstabe)
                                in
                                    if List.member echterBuchstabe model.erraten then
                                        Entschlüsselt zufälligerBuchstabe
                                    else
                                        Verschlüsselt zufälligerBuchstabe
                            else
                                verschluessleDenBuchstaben model echterBuchstabe (indexInDerTabelle + 1)



-- UPDATE


type Msg
    = BuchstabeGeraten Int String
    | NeuerVerschlüsselterBuchstabe String Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BuchstabeGeraten derWievielteBuchstabe geratenerBuchstabe ->
            let
                echterBuchstabe =
                    String.toUpper (String.slice derWievielteBuchstabe (derWievielteBuchstabe + 1) model.echterSatz)
            in
                if String.toUpper geratenerBuchstabe == echterBuchstabe then
                    ( { model | erraten = model.erraten ++ [ echterBuchstabe ] }, Cmd.none )
                else
                    ( { model | anzahlDerFehler = model.anzahlDerFehler + 1 }, Cmd.none )

        NeuerVerschlüsselterBuchstabe echterBuchstabe codeDesZufälligenBuchstabens ->
            if List.member codeDesZufälligenBuchstabens model.bereitsVorhandeneVerschlüsselungen then
                ( model, Random.generate (NeuerVerschlüsselterBuchstabe echterBuchstabe) (Random.int 1 6) )
            else
                ( { model
                    | bereitsVorhandeneVerschlüsselungen = model.bereitsVorhandeneVerschlüsselungen ++ [ codeDesZufälligenBuchstabens ]
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
        , zeigeAnzahlDerFehler model
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
                                , ( "min-width", "1.2em" )
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
            verschluessleDenBuchstaben model (String.toUpper echterBuchstabe) 0
    in
        case verschlüsselungsergebnis of
            NichtVerschlüsselt ->
                [ text echterBuchstabe ]

            Verschlüsselt zufälligerBuchstabe ->
                [ text zufälligerBuchstabe
                , Html.br [] []
                , Html.input
                    [ placeholder "?"
                    , attribute "onfocus" "this.select()"
                    , value ""
                      -- bereits eingegebene falsche Zeichen löschen
                    , style
                        [ ( "width", "1em" )
                        , ( "text-align", "center" )
                        ]
                    , onInput (BuchstabeGeraten aktuellerBuchstabenIndex)
                    ]
                    []
                ]

            Entschlüsselt zufälligerBuchstabe ->
                [ text zufälligerBuchstabe
                , Html.br [] []
                , span
                    [ style
                        [ ( "color", "green" )
                        , ( "font-weight", "bold" )
                        ]
                    ]
                    [ text echterBuchstabe ]
                ]


zeigeAnzahlDerFehler : Model -> Html.Html Msg
zeigeAnzahlDerFehler model =
    let
        message =
            if model.anzahlDerFehler > 0 then
                "Du hast schon " ++ (toString model.anzahlDerFehler) ++ "x falsch geraten."
            else
                ""
    in
        div [ style [ ( "color", "red" ) ] ] [ text message ]
