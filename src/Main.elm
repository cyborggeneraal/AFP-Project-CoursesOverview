module Main exposing (..)

import Browser
import Html exposing (Html, div, text, table, thead, tr, th, tbody, td)
import Html.Attributes exposing (style)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, string, list, map6)
import Html exposing (button)
import Html.Events exposing (onClick)

type alias Course =
    { term : Int
    , timeSlot : String
    , courseID : Int
    , level : String
    , ecName : String
    , capacity : Int
    }

type alias Model =
    { courses : List Course
    , error : Maybe String
    }

type Msg
    = FetchCourses
    | ReceiveCourses (Result Http.Error (List Course))

init : Model
init = { courses = [], error = Nothing }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FetchCourses ->
            (model, fetchCourses)

        ReceiveCourses result ->
            case result of
                Ok courses ->
                    ({ model | courses = courses, error = Nothing }, Cmd.none)

                Err _ ->
                    ({ model | error = Just "An error occurred while fetching courses." }, Cmd.none)

fetchCourses : Cmd Msg
fetchCourses =
    Http.get
        { url = "http://localhost:8081/courses"
        , expect = Http.expectJson ReceiveCourses coursesDecoder
        }

coursesDecoder : Decoder (List Course)
coursesDecoder =
    Decode.list courseDecoder

courseDecoder : Decoder Course
courseDecoder =
    Decode.map6 Course
        (field "term" Decode.int)
        (field "timeSlot" Decode.string)
        (field "courseID" Decode.int)
        (field "level" Decode.string)
        (field "ecName" Decode.string)
        (field "capacity" Decode.int)

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchCourses ] [ text "Fetch Courses" ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Term" ]
                    , th [] [ text "Time Slot" ]
                    , th [] [ text "Course ID" ]
                    , th [] [ text "Level" ]
                    , th [] [ text "Course Name" ]
                    , th [] [ text "Capacity" ]
                    ]
                ]
            , tbody [] (List.map courseRow model.courses)
            ]
        ]

courseRow : Course -> Html Msg
courseRow course =
    tr []
        [ td [] [ text (String.fromInt course.term) ]
        , td [] [ text course.timeSlot ]
        , td [] [ text (String.fromInt course.courseID) ]
        , td [] [ text course.level ]
        , td [] [ text course.ecName ]
        , td [] [ text (String.fromInt course.capacity) ]
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (init, fetchCourses)
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }