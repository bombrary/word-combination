module Main exposing (..)

import Adjective
import Array exposing (Array)
import Browser
import Browser.Dom
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Idea exposing (Idea)
import Ideas exposing (Ideas)
import Json.Decode as JD
import Noun
import Random
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Task



-- Main


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { nouns : Ideas
    , adjectives : Ideas
    , nounsOrigin : Array String
    , adjectivesOrigin : Array String
    , focusedInput : Maybe FocusedInput
    }


type FocusedInput
    = NounInput Int
    | AdjectiveInput Int


type Key
    = Enter
    | Other String


type alias Keyboard =
    { key : Key
    , shift : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nouns = Ideas.repeat 20 Nothing
      , adjectives = Ideas.repeat 10 Nothing
      , nounsOrigin = Noun.words
      , adjectivesOrigin = Adjective.words
      , focusedInput = Nothing
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | OnNounChanged Int String
    | OnAdjectiveChanged Int String
    | OnNounsButtonClicked
    | OnAdjectivesButtonClicked
    | NewNouns Ideas
    | NewAdjectives Ideas
    | OnFocusedInput FocusedInput
    | OnKeyPressed Keyboard


focusedInputToId : FocusedInput -> String
focusedInputToId focusedInput =
    case focusedInput of
        NounInput i ->
            "noun-input-" ++ String.fromInt i

        AdjectiveInput i ->
            "adjective-input-" ++ String.fromInt i


focusInput : FocusedInput -> Cmd Msg
focusInput focusedInput =
    Task.attempt
        (\res ->
            case res of
                Ok _ ->
                    OnFocusedInput focusedInput

                Err _ ->
                    NoOp
        )
        (Browser.Dom.focus (focusedInputToId focusedInput))


moveFocus : Int -> FocusedInput -> Cmd Msg
moveFocus d focusedInput =
    case focusedInput of
        NounInput i ->
            focusInput (NounInput (i + d))

        AdjectiveInput i ->
            focusInput (AdjectiveInput (i + d))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnNounChanged i string ->
            ( { model | nouns = Ideas.set i string model.nouns }
            , Cmd.none
            )

        OnAdjectiveChanged i string ->
            ( { model | adjectives = Ideas.set i string model.adjectives }
            , Cmd.none
            )

        OnNounsButtonClicked ->
            ( model
            , Random.generate NewNouns (Ideas.choose 20 model.nounsOrigin)
            )

        OnAdjectivesButtonClicked ->
            ( model
            , Random.generate NewAdjectives (Ideas.choose 10 model.adjectivesOrigin)
            )

        NewNouns nouns ->
            ( { model | nouns = nouns }
            , Cmd.none
            )

        NewAdjectives adjectives ->
            ( { model | adjectives = adjectives }
            , Cmd.none
            )

        OnFocusedInput focusedInput ->
            ( { model | focusedInput = Just focusedInput }
            , Cmd.none
            )

        OnKeyPressed keyboard ->
            let
                cmd =
                    case model.focusedInput of
                        Nothing ->
                            Cmd.none

                        Just focusedInput ->
                            case keyboard.key of
                                Other _ ->
                                    Cmd.none

                                Enter ->
                                    if keyboard.shift then
                                        moveFocus -1 focusedInput

                                    else
                                        moveFocus 1 focusedInput
            in
            ( model, cmd )



-- View


view : Model -> Html Msg
view model =
    div []
        [ viewHero model
        , viewWordCombination model
        ]



--- ViewHero


viewHero : Model -> Svg Msg
viewHero model =
    img
        [ src "hero.svg"
        , style "width" "100%"
        ]
        []



-- ViewWordCombination


viewWordCombination : Model -> Html Msg
viewWordCombination model =
    div [ class "word-combination-main" ]
        [ div [ class "description" ]
            [ p [] [ text "名刺と形容詞のすべての組み合わせを列挙します。単語は自分で入力することもできますが、「ランダム」ボタンを押すとランダムに単語が生成されます。"]
            , p [] [ text "物語のアイデアを考えるお供にお使いください。" ]
            , p [] [ text "単語データは"
                   , a [ href "https://bond-lab.github.io/wnja/" ]
                       [ text "日本語Wordnet"]
                   , text "から取得しました。"
                   ]
            ]
        , div [ class "input-nouns-outer" ]
            [ h2 [ id "#noun" ] [ text "名詞" ]
            , viewNoun model
            , button [ onClick OnNounsButtonClicked ] [ text "ランダム" ]
            ]
        , div [ class "input-adjectives-outer" ]
            [ h2 [ id "#adjective" ] [ text "形容詞" ]
            , viewAdjective model
            , button [ onClick OnAdjectivesButtonClicked ] [ text "ランダム" ]
            ]
        , div [ class "output-ideas-outer" ]
            [ h2 [ id "#ideas" ] [ text "生まれたアイデア" ]
            , viewIdeas model
            ]
        ]


viewNoun : Model -> Html Msg
viewNoun model =
    div [ class "input-nouns-inner" ]
        (List.indexedMap viewNounInput (Ideas.toList model.nouns))


viewNounInput : Int -> Idea -> Html Msg
viewNounInput i noun =
    input
        [ type_ "text"
        , onInput (OnNounChanged i)
        , value (Idea.withDefault "" noun)
        , onFocus (OnFocusedInput (NounInput i))
        , id ("noun-input-" ++ String.fromInt i)
        ]
        []


viewAdjective : Model -> Html Msg
viewAdjective model =
    div [ class "input-nouns-inner" ]
        (List.indexedMap viewAdjectiveInput (Ideas.toList model.adjectives))


viewAdjectiveInput : Int -> Idea -> Html Msg
viewAdjectiveInput i adjective =
    input
        [ type_ "text"
        , onInput (OnAdjectiveChanged i)
        , value (Idea.withDefault "" adjective)
        , onFocus (OnFocusedInput (AdjectiveInput i))
        , id ("adjective-input-" ++ String.fromInt i)
        ]
        []


viewIdeas : Model -> Html Msg
viewIdeas model =
    div [ class "ideas-output" ]
        (List.map viewIdea
            (Array.toList <|
                Ideas.values
                    (Ideas.compose model.adjectives model.nouns)
            )
        )


viewIdea : String -> Html Msg
viewIdea string =
    p [] [ text string ]



-- Subscriptions


keyDecoder : JD.Decoder Msg
keyDecoder =
    JD.map2 toKey
        (JD.field "key" JD.string)
        (JD.field "shiftKey" JD.bool)


toKey : String -> Bool -> Msg
toKey keyCode shift =
    let
        key =
            if keyCode == "Enter" then
                Enter

            else
                Other keyCode
    in
    OnKeyPressed
        { key = key
        , shift = shift
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyPress keyDecoder
