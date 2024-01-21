module Route.Blog.Slug_ exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import BackendTask.File
import BackendTask.Glob
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Json.Decode as Decode
import Markdown.Block exposing (Block)
import Markdown.Parser
import Markdown.Renderer
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Parser exposing (Problem)
import Parser.Advanced exposing (DeadEnd)
import RouteBuilder exposing (App, StatelessRoute)
import Shared
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    { slug : String
    }


type alias Data =
    List Block


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.preRender
        { head = head
        , pages = pages
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }


pages : BackendTask FatalError (List RouteParams)
pages =
    BackendTask.Glob.succeed RouteParams
        |> BackendTask.Glob.match (BackendTask.Glob.literal "blog/")
        |> BackendTask.Glob.capture BackendTask.Glob.wildcard
        |> BackendTask.Glob.match (BackendTask.Glob.literal ".md")
        |> BackendTask.Glob.toBackendTask


type alias ActionData =
    {}


data : RouteParams -> BackendTask FatalError Data
data params =
    let
        fullPath =
            ""
                ++ "blog/"
                ++ params.slug
                ++ ".md"

        deadEndsToString : List (DeadEnd String Problem) -> String
        deadEndsToString deadEnds =
            deadEnds
                |> List.map Markdown.Parser.deadEndToString
                |> String.join "\n"
    in
    BackendTask.File.bodyWithFrontmatter
        (\raw ->
            let
                parsed =
                    Markdown.Parser.parse raw
                        |> Result.mapError
                            (\err ->
                                "Could not parse markdown in: "
                                    ++ fullPath
                                    ++ "\n"
                                    ++ deadEndsToString err
                            )
            in
            case parsed of
                Err err ->
                    Decode.fail err

                Ok blocks ->
                    Decode.succeed blocks
        )
        fullPath
        |> BackendTask.mapError (\err -> err.fatal)


head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> View (PagesMsg Msg)
view app sharedModel =
    { title = "Placeholder - Blog.Slug_"
    , body =
        let
            rendered =
                Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer app.data
        in
        case rendered of
            Err err ->
                [ Html.text err ]

            Ok htmlBlocks ->
                List.map (Html.map PagesMsg.fromMsg) htmlBlocks
    }
