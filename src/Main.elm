module Main exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Bytes.Encode
import Dict
import Dict.Extra exposing (groupBy)
import Element
    exposing
        ( Element
        , alignBottom
        , alignRight
        , alpha
        , centerX
        , centerY
        , clip
        , clipY
        , el
        , fill
        , focused
        , height
        , mouseOver
        , moveLeft
        , moveRight
        , moveUp
        , padding
        , paddingEach
        , paddingXY
        , px
        , rgb255
        , row
        , scrollbarY
        , scrollbars
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes exposing (attribute, style)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import RemoteData exposing (..)
import Set exposing (Set)
import Task


server_url =
    "http://localhost:8000"


type alias Model =
    { searchText : String
    , bodyText : String
    , selectedIds : Set Int
    , outline : Outline
    , assets : WebData (List Asset)
    , tags : WebData (List Tag)
    , assetTags : WebData (List AssetTag)
    , activeTab : Tab
    , uploadFile : Maybe File
    , infoPanelKey : String
    , infoPanelValue : String
    }


type Msg
    = WebResponse (Result Http.Error String)
    | WebRequestButtonPressed
    | SearchFieldUpdated String
    | SelectedAsset Int
    | ReceivedAssetInfo (WebData (List Asset))
    | ReceivedTagInfo (WebData (List Tag))
    | ReceivedAssetTagInfo (WebData (List AssetTag))
    | PressedUploadTab
    | PressedSearchTab
    | FileRequested
    | FileSelected File
    | TestUpload
    | UploadedAsset (Result Http.Error Int)
    | OutlineRefreshRequested
    | TagCreatedPostAssetIntent Int (Result Http.Error Int)
    | CreateTagForAsset String String Int
    | PostTagOnAssetResult (Result Http.Error ())
    | DeleteTagOnAssetResult (Result Http.Error ())
    | RemovedTagFromAsset AssetTag
    | AddTagToAssetRequested AssetTag
    | InfoPanelAddTagPressed
    | InfoPanelKeyUpdated String
    | InfoPanelValueUpdated String


type Tab
    = SearchTab
    | UploadTab


type alias Asset =
    { name : String
    , id : Int
    }


type alias Tag =
    { key : String
    , value : String
    , id : Int
    }


type alias AssetTag =
    { assetId : Int
    , tagId : Int
    }


type alias AssetOutlineInfo =
    { name : String
    , id : Int
    }


type alias Outline =
    { assets : List AssetOutlineInfo
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { searchText = ""
      , assets = NotAsked
      , tags = NotAsked
      , assetTags = NotAsked
      , bodyText = ""
      , selectedIds = Set.empty |> Set.insert 20
      , outline = { assets = [] }
      , activeTab = SearchTab
      , uploadFile = Nothing
      , infoPanelKey = ""
      , infoPanelValue = ""
      }
    , Cmd.batch [ httpGetAssets, httpGetTags, httpGetAssetTags ]
    )


httpGetAssets =
    Http.get
        { url = "http://localhost:8000/assets"
        , expect = Http.expectJson (RemoteData.fromResult >> ReceivedAssetInfo) decodeAssets
        }


httpGetTags : Cmd Msg
httpGetTags =
    Http.get
        { url = "http://localhost:8000/tags"
        , expect = Http.expectJson (RemoteData.fromResult >> ReceivedTagInfo) decodeTags
        }


httpGetAssetTags : Cmd Msg
httpGetAssetTags =
    Http.get
        { url = "http://localhost:8000/asset_tags"
        , expect = Http.expectJson (RemoteData.fromResult >> ReceivedAssetTagInfo) decodeAssetTags
        }


httpPostTag : String -> String -> (Result Http.Error Int -> Msg) -> Cmd Msg
httpPostTag key value msg =
    Http.post
        { url = server_url ++ "/tags"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "key", Json.Encode.string key )
                    , ( "value", Json.Encode.string value )
                    ]
                )
        , expect = Http.expectJson msg decodeUpload
        }


httpPostTagOnAsset assetId tagId =
    Http.post
        { url = server_url ++ "/assets/" ++ String.fromInt assetId ++ "/tags"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "tag_id", Json.Encode.int tagId ) ]
                )
        , expect = Http.expectWhatever PostTagOnAssetResult
        }


httpDelete data =
    Http.request
        { method = "DELETE"
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = data.url
        , expect = data.expect
        }


httpDeleteAssetTag assetTag =
    httpDelete
        { url = server_url ++ "/assets/" ++ String.fromInt assetTag.assetId ++ "/tags/" ++ String.fromInt assetTag.tagId
        , expect = Http.expectWhatever DeleteTagOnAssetResult
        }


decodeAsset : Decoder Asset
decodeAsset =
    Json.Decode.map2 (\name id -> { name = name, id = id })
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "id" Json.Decode.int)


decodeAssets : Decoder (List Asset)
decodeAssets =
    Json.Decode.field "asset" (Json.Decode.list decodeAsset)


decodeTag : Decoder Tag
decodeTag =
    Json.Decode.map3 Tag
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "value" Json.Decode.string)
        (Json.Decode.field "tag_id" Json.Decode.int)


decodeTags : Decoder (List Tag)
decodeTags =
    Json.Decode.list decodeTag


decodeAssetTag : Decoder AssetTag
decodeAssetTag =
    Json.Decode.map2 AssetTag
        (Json.Decode.field "asset_id" Json.Decode.int)
        (Json.Decode.field "tag_id" Json.Decode.int)


decodeAssetTags : Decoder (List AssetTag)
decodeAssetTags =
    Json.Decode.list decodeAssetTag


decodeUpload : Decoder Int
decodeUpload =
    Json.Decode.field "id" Json.Decode.int


type UpsertAction
    = CreateTag String String Int
    | PostTag AssetTag
    | BadUpsert


upsertActionToMsg : UpsertAction -> Maybe Msg
upsertActionToMsg action =
    case action of
        CreateTag key value assetId ->
            Just (CreateTagForAsset key value assetId)

        PostTag assetTag ->
            Just (AddTagToAssetRequested { assetId = assetTag.assetId, tagId = assetTag.tagId })

        BadUpsert ->
            Nothing


upsertActionToCmd : UpsertAction -> Cmd Msg
upsertActionToCmd action =
    case action of
        CreateTag key value assetId ->
            httpPostTag key value (TagCreatedPostAssetIntent assetId)

        PostTag assetTag ->
            httpPostTagOnAsset assetTag.assetId assetTag.tagId

        BadUpsert ->
            Cmd.none


upsertAssetTag : Int -> String -> String -> Model -> UpsertAction
upsertAssetTag assetId key value model =
    model.tags
        |> RemoteData.map (List.filter (\tag -> tag.key == key))
        |> RemoteData.map (List.filter (\tag -> tag.value == value))
        |> RemoteData.map List.head
        |> (\result ->
                case result of
                    -- The tag exists, let's just post it on the new asset
                    Success (Just tag) ->
                        PostTag { assetId = assetId, tagId = tag.id }

                    -- We've loaded the tags, but can't find the one we're looking for
                    Success Nothing ->
                        CreateTag key value assetId

                    -- We haven't even asked for tag information yet...
                    -- Ideally we'd start loading the tags and queue this
                    -- to run once the tags load
                    NotAsked ->
                        BadUpsert

                    -- We're loading the tag information
                    -- Ideally we'd queue this to run once the tags load
                    Loading ->
                        BadUpsert

                    -- Error message? It's interesting that we'd succeed
                    -- in uploading, but failed to load the tags
                    Failure _ ->
                        BadUpsert
           )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RemovedTagFromAsset assetTag ->
            ( model, httpDeleteAssetTag assetTag )

        PostTagOnAssetResult (Ok _) ->
            ( model, httpGetAssetTags )

        PostTagOnAssetResult (Err e) ->
            ( { model | bodyText = model.bodyText ++ " Error when posting tag on asset" ++ Debug.toString e }, Cmd.none )

        DeleteTagOnAssetResult (Ok _) ->
            ( { model | assetTags = Loading }, httpGetAssetTags )

        DeleteTagOnAssetResult (Err e) ->
            ( { model | bodyText = model.bodyText ++ " Error when deleting tag on asset" ++ Debug.toString e }, Cmd.none )

        UploadedAsset (Ok assetId) ->
            ( model
            , model.tags
                |> RemoteData.map (List.filter (\tag -> tag.key == "mime-type"))
                |> RemoteData.map
                    (List.filter
                        (\tag ->
                            tag.value
                                == (model.uploadFile
                                        |> Maybe.map File.mime
                                        |> Maybe.withDefault "application/octet-stream"
                                   )
                        )
                    )
                |> RemoteData.map List.head
                |> (\result ->
                        case result of
                            -- The tag exists, let's just post it on the new asset
                            Success (Just tag) ->
                                httpPostTagOnAsset assetId tag.id

                            -- We've loaded the tags, but can't find the one we're looking for
                            Success Nothing ->
                                httpPostTag "mime-type"
                                    (model.uploadFile
                                        |> Maybe.map File.mime
                                        |> Maybe.withDefault "application/octet-stream"
                                    )
                                    (TagCreatedPostAssetIntent assetId)

                            -- We haven't even asked for tag information yet...
                            -- Ideally we'd start loading the tags and queue this
                            -- to run once the tags load
                            NotAsked ->
                                Cmd.none

                            -- We're loading the tag information
                            -- Ideally we'd queue this to run once the tags load
                            Loading ->
                                Cmd.none

                            -- Error message? It's interesting that we'd succeed
                            -- in uploading, but failed to load the tags
                            Failure _ ->
                                Cmd.none
                   )
            )

        InfoPanelKeyUpdated key ->
            ( { model | infoPanelKey = key }, Cmd.none )

        InfoPanelValueUpdated value ->
            ( { model | infoPanelValue = value }, Cmd.none )

        UploadedAsset err ->
            ( { model | bodyText = model.bodyText ++ Debug.toString err }, Cmd.none )

        CreateTagForAsset key value assetId ->
            ( model, httpPostTag key value (TagCreatedPostAssetIntent assetId) )

        TagCreatedPostAssetIntent assetId (Ok tagId) ->
            ( model, Cmd.batch [ httpGetTags, httpPostTagOnAsset assetId tagId ] )

        TagCreatedPostAssetIntent _ (Err e) ->
            ( { model | bodyText = model.bodyText ++ Debug.toString e }, Cmd.none )

        AddTagToAssetRequested assetTag ->
            ( model, httpPostTagOnAsset assetTag.assetId assetTag.tagId )

        InfoPanelAddTagPressed ->
            ( { model | infoPanelKey = "", infoPanelValue = "" }
            , case Set.toList model.selectedIds of
                [ assetId ] ->
                    upsertAssetTag assetId model.infoPanelKey model.infoPanelValue model
                        |> upsertActionToCmd

                _ ->
                    Cmd.none
            )

        TestUpload ->
            case model.uploadFile of
                Nothing ->
                    ( model, Cmd.none )

                Just file ->
                    ( model
                    , Http.post
                        { url = server_url ++ "/assets"
                        , body =
                            Http.multipartBody
                                [ Http.filePart "file" file
                                ]
                        , expect = Http.expectJson UploadedAsset decodeUpload
                        }
                    )

        SelectedAsset assetId ->
            ( { model | selectedIds = Set.empty |> Set.insert assetId }
            , Cmd.none
            )

        SearchFieldUpdated s ->
            ( { model | searchText = s }, Cmd.none )

        WebRequestButtonPressed ->
            ( model
            , Http.get
                { url = "http://neverssl.com/"
                , expect = Http.expectString WebResponse
                }
            )

        OutlineRefreshRequested ->
            ( model, httpGetAssets )

        FileRequested ->
            ( model, Select.file [ "text/csv" ] FileSelected )

        FileSelected file ->
            ( { model | uploadFile = Just file }, Cmd.none )

        PressedUploadTab ->
            ( { model | activeTab = UploadTab }, Cmd.none )

        PressedSearchTab ->
            ( { model | activeTab = SearchTab }, Cmd.none )

        WebResponse (Ok s) ->
            ( { model | bodyText = model.bodyText ++ s }, Cmd.none )

        WebResponse (Err e) ->
            ( { model | bodyText = model.bodyText ++ "Got an error" }, Cmd.none )

        ReceivedAssetInfo assetResult ->
            case assetResult of
                Success assets ->
                    ( { model | assets = Success assets }, Cmd.none )

                _ ->
                    ( { model | bodyText = model.bodyText ++ "\n" ++ Debug.toString assetResult }, Cmd.none )

        ReceivedTagInfo tagResult ->
            case tagResult of
                Success tags ->
                    ( { model | tags = Success tags }, Cmd.none )

                _ ->
                    ( { model | bodyText = model.bodyText ++ "\n" ++ Debug.toString tagResult }, Cmd.none )

        ReceivedAssetTagInfo (Success assetTags) ->
            ( { model | assetTags = Success assetTags }, Cmd.none )

        ReceivedAssetTagInfo other ->
            ( { model | bodyText = model.bodyText ++ "\n" ++ Debug.toString other }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewHeader : Model -> Element Msg
viewHeader model =
    Element.el
        [ Element.htmlAttribute (style "-webkit-app-region" "drag")
        , Background.color (rgb255 35 35 35)
        , width fill
        , height (px 26)
        , Font.size 12
        , paddingXY 9 0
        ]
        (Element.row
            [ height fill, width fill ]
            [ Element.el
                [ height fill
                , paddingEach { left = 0, right = 40, top = 0, bottom = 0 }
                ]
                (Element.el
                    [ centerY ]
                    (text "SHAM")
                )
            , Element.row [ spacing 4, height fill ]
                [ Input.button
                    [ if model.activeTab == SearchTab then
                        Background.color (rgb255 58 58 58)

                      else
                        Background.color (rgb255 43 43 43)
                    , Element.htmlAttribute (style "-webkit-app-region" "no-drag")
                    , mouseOver [ Background.color (rgb255 43 43 243) ]
                    , paddingEach { left = 8, right = 8, top = 6, bottom = 4 }
                    , alignBottom
                    , Font.color (rgb255 152 152 152)
                    , Border.roundEach
                        { bottomLeft = 0
                        , bottomRight = 0
                        , topLeft = 4
                        , topRight = 4
                        }
                    ]
                    { onPress = Just PressedSearchTab
                    , label =
                        Element.el
                            []
                            (text "Search")
                    }
                , Input.button
                    [ if model.activeTab == UploadTab then
                        Background.color (rgb255 58 58 58)

                      else
                        Background.color (rgb255 43 43 43)
                    , Element.htmlAttribute (style "-webkit-app-region" "no-drag")
                    , mouseOver [ Background.color (rgb255 43 43 243) ]
                    , paddingEach { left = 8, right = 8, top = 6, bottom = 4 }
                    , alignBottom
                    , Font.color (rgb255 152 152 152)
                    , Border.roundEach
                        { bottomLeft = 0
                        , bottomRight = 0
                        , topLeft = 4
                        , topRight = 4
                        }
                    ]
                    { onPress = Just PressedUploadTab
                    , label =
                        Element.el
                            []
                            (text "Upload")
                    }
                ]
            , Element.el
                [ height fill
                , width fill
                , alignRight
                ]
                (Element.el
                    [ centerY, width fill, alignRight, Font.alignRight ]
                    (text "X")
                )
            ]
        )


viewBody : Model -> Element Msg
viewBody model =
    case model.activeTab of
        SearchTab ->
            Element.row
                [ width fill
                , paddingXY 1 0
                , spacing 3
                , height fill
                , scrollbarY
                ]
                [ viewSearchPanel model
                , viewInfoPanel model
                ]

        UploadTab ->
            Element.el
                [ width fill
                , height fill
                , paddingXY 1 0
                ]
                (viewUploadPanel model)


viewUploadPanel : Model -> Element Msg
viewUploadPanel model =
    Element.el
        [ Border.roundEach
            { bottomLeft = 8
            , bottomRight = 8
            , topLeft = 8
            , topRight = 8
            }
        , Background.color (rgb255 58 58 58)
        , width fill
        , height fill
        , scrollbarY
        ]
        (Element.column
            []
            (case model.uploadFile of
                Nothing ->
                    [ Input.button
                        [ Border.width 1 ]
                        { onPress = Just FileRequested
                        , label = text "Upload File"
                        }
                    , text model.bodyText
                    ]

                Just _ ->
                    [ Input.button
                        []
                        { onPress = Just TestUpload
                        , label = text "Test Upload"
                        }
                    , text model.bodyText
                    ]
            )
        )


viewSearchPanel : Model -> Element Msg
viewSearchPanel model =
    Element.el
        [ Border.roundEach
            { bottomLeft = 8
            , bottomRight = 8
            , topLeft = 8
            , topRight = 8
            }
        , Background.color (rgb255 58 58 58)
        , width fill
        , height fill
        , scrollbarY
        ]
        (Element.column
            [ width fill
            , height fill
            , scrollbarY
            ]
            [ Element.el [ width fill ] (text model.bodyText)
            , Element.el [ width fill ] (text (Debug.toString model.tags))
            , Element.el [ width fill ] (text "Search")
            , Element.el [ width fill ] (text "or something")
            , Input.button
                [ width fill
                , focused []
                , Border.width 5
                ]
                { onPress = Just OutlineRefreshRequested
                , label =
                    Element.el
                        [ width fill
                        ]
                        (text "refresh")
                }
            , Element.column
                [ width fill
                , height fill
                , scrollbarY
                ]
                (viewAssetOutline model)
            ]
        )


viewAssetOutline : Model -> List (Element Msg)
viewAssetOutline model =
    let
        assetInfo =
            model.assets
                |> RemoteData.withDefault []

        tagIndex =
            model.tags
                |> RemoteData.withDefault []
                |> List.map (\tag -> ( tag.id, tag ))
                |> Dict.fromList

        tagsByAssetId =
            model.assetTags
                |> RemoteData.withDefault []
                |> groupBy .assetId

        assetWithTags =
            assetInfo
                |> List.map
                    (\asset ->
                        { asset = asset
                        , tags =
                            Dict.get asset.id tagsByAssetId
                                |> Maybe.withDefault []
                                |> List.map (\assetTag -> Dict.get assetTag.tagId tagIndex)
                                |> List.filterMap identity
                        }
                    )
    in
    List.indexedMap
        (\n info ->
            Input.button
                [ width fill
                , height (px 31)
                , focused []
                , clip
                ]
                { onPress = Just (SelectedAsset info.asset.id)
                , label =
                    Element.el
                        [ width fill
                        , height (px 31)
                        , clip
                        , mouseOver
                            [ Background.color (rgb255 40 40 240)
                            ]
                        , case ( Set.member info.asset.id model.selectedIds, modBy 2 n == 0 ) of
                            ( True, _ ) ->
                                Background.color (rgb255 40 40 140)

                            ( False, True ) ->
                                Background.color (rgb255 40 40 40)

                            ( False, False ) ->
                                Background.color (rgb255 45 45 45)
                        ]
                        (Element.row [ centerY, spacing 6 ]
                            ([ Element.el [ centerY ]
                                (text info.asset.name)
                             ]
                                ++ (case info.tags of
                                        [] ->
                                            []

                                        [ first ] ->
                                            [ viewTagChit [] first ]

                                        [ first, second ] ->
                                            [ viewTagChit [] first, viewTagChit [] second ]

                                        first :: second :: rest ->
                                            List.map (viewTagChit []) (List.take 2 info.tags)
                                                ++ [ viewExtraChit (List.length info.tags - 2) ]
                                   )
                            )
                        )
                }
        )
        assetWithTags


viewTagChit : List (Element.Attribute Msg) -> Tag -> Element Msg
viewTagChit attrs tag =
    Element.row
        ([ Font.size 10
         , Border.width 1
         , width shrink
         ]
            ++ attrs
        )
        [ Element.el
            [ Background.color (rgb255 220 220 220)
            , Font.color (rgb255 40 40 40)
            , paddingXY 2 0
            ]
            (text tag.key)
        , Element.el [ paddingXY 2 0 ] (text tag.value)
        ]


viewExtraChit : Int -> Element Msg
viewExtraChit count =
    Element.row
        [ Font.size 10
        , Border.width 1
        ]
        [ Element.el
            [ Background.color (rgb255 220 220 220)
            , Font.color (rgb255 40 40 40)
            , paddingXY 2 0
            ]
            (text ("+" ++ String.fromInt count))

        --, Element.el [ paddingXY 2 0 ] (text tag.value)
        ]


viewInfoPanel : Model -> Element Msg
viewInfoPanel model =
    Element.column
        [ Border.roundEach
            { bottomLeft = 8
            , bottomRight = 8
            , topLeft = 8
            , topRight = 8
            }
        , Background.color (rgb255 58 58 58)
        , width (px 500)
        , height fill
        ]
        ((case Set.toList model.selectedIds of
            [] ->
                [ text "Nothing Selected" ]

            [ assetId ] ->
                case ( model.tags, model.assetTags ) of
                    ( Success tags, Success assetTags ) ->
                        let
                            filteredAssetTags =
                                assetTags
                                    |> List.filter (\assetTag -> assetTag.assetId == assetId)
                                    |> List.map (\assetTag -> assetTag.tagId)

                            filteredTags =
                                tags
                                    |> List.filter (\tag -> List.member tag.id filteredAssetTags)
                        in
                        [ Element.el
                            [ paddingXY 8 8
                            , width fill
                            ]
                            (Element.el
                                [ width fill
                                , height (px 500)
                                , Background.color (rgb255 45 45 45)
                                , Border.rounded 8
                                , scrollbars
                                ]
                                (filteredTags
                                    |> List.filter (\tag -> tag.key == "mime-type")
                                    |> List.head
                                    |> Maybe.map
                                        (\tag ->
                                            Element.html
                                                (Html.node "asset-preview"
                                                    [ attribute "mime-type" tag.value
                                                    , attribute "src" (server_url ++ "/assets/" ++ String.fromInt assetId)
                                                    ]
                                                    []
                                                )
                                        )
                                    |> Maybe.withDefault Element.none
                                )
                            )
                        , Element.paragraph []
                            (filteredTags
                                |> List.map
                                    (\tag ->
                                        viewTagChit
                                            [ Element.inFront
                                                (Input.button
                                                    [ Font.color (rgb255 200 200 200)
                                                    , Background.color (rgb255 60 60 60)
                                                    , alpha 0.0
                                                    , mouseOver [ alpha 1.0 ]
                                                    , Border.rounded 10000
                                                    , centerX
                                                    , centerY
                                                    , Border.width 1
                                                    , Font.center
                                                    , Font.size 8
                                                    , width (px 16)
                                                    , height (px 16)
                                                    , alignRight
                                                    , Element.htmlAttribute (style "z-index" "100")
                                                    , moveUp 8
                                                    , moveRight 8
                                                    ]
                                                    { onPress = Just (RemovedTagFromAsset { tagId = tag.id, assetId = assetId })
                                                    , label =
                                                        Element.el
                                                            [ centerX, centerY ]
                                                            (Element.el [ centerY, centerX ]
                                                                (text "X")
                                                            )
                                                    }
                                                )
                                            ]
                                            tag
                                    )
                            )
                        ]

                    _ ->
                        [ text "Loading tags..." ]

            _ ->
                [ text "Multiple Selected" ]
         )
            ++ [ case Set.toList model.selectedIds of
                    [] ->
                        text "Nothing Selected"

                    [ assetId ] ->
                        case ( model.tags, model.assetTags ) of
                            ( Success tags, Success assetTags ) ->
                                Element.column [ spacing 4 ]
                                    [ Input.text []
                                        { label = Input.labelLeft [] (text "key")
                                        , onChange = InfoPanelKeyUpdated
                                        , placeholder = Nothing
                                        , text = model.infoPanelKey
                                        }
                                    , Input.text []
                                        { label = Input.labelLeft [] (text "value")
                                        , onChange = InfoPanelValueUpdated
                                        , placeholder = Nothing
                                        , text = model.infoPanelValue
                                        }
                                    , Input.button []
                                        { onPress = Just InfoPanelAddTagPressed
                                        , label = text "Add Tag"
                                        }
                                    ]

                            _ ->
                                text "Loading tags..."

                    _ ->
                        text "Multiple Selected"
               ]
        )



-- InfoPanelKeyUpdated key ->
-- InfoPanelValueUpdated value ->


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color (rgb255 31 31 31)
        , Font.color (rgb255 220 220 220)
        , height fill
        , scrollbarY
        ]
    <|
        Element.column
            [ width fill
            , height fill
            , scrollbarY
            ]
            [ viewHeader model
            , viewBody model
            ]


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
