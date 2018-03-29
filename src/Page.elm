module Page exposing (main)

import Mouse
import WebGL exposing (Mesh, Shader)
import WebGL.Settings.Blend as Blend
import WebGL.Texture as Texture exposing (Texture, Error)
import Math.Vector4 exposing (Vec4, vec4)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector2 exposing (Vec2, vec2)
import Task exposing (Task)
import AnimationFrame
import Window
import Html.Attributes exposing (width, height, style)
import Html exposing (Html)
import Time exposing (Time)
import List
import Sound

{-| Types
-}
type Action
    = Resize Window.Size
    | MouseClicked Mouse.Position
    | Animate Time
    | TextureLoad Texture
    | TextureError Error

type Model = InitialPage ForInitial | Playing ForPlay

type alias ForInitial =
    { size : Window.Size
    , maybeTexture : Maybe WebGL.Texture
    }

type alias ForPlay =
    { size : Window.Size
    , position : Mouse.Position
    , freq : Float
    , texture : WebGL.Texture
    , elapsed : Time
    , vols : List Float
    }


{-| Program -}

numWave = 8
periodMin = 0.5
periodMax = 1.0

period : Int -> Float
period i = periodMin + (periodMax - periodMin) * (toFloat i) / (toFloat (numWave - 1))

slope : Int -> Float
slope i = 1.0 / (period i)

envelope : Float -> Int -> Float -> Float
envelope dt i prev = prev - dt * slope i

play : Float -> Float -> Int -> Cmd Action
play currentTime baseFreq i =
    let dur = period i in
      Sound.play currentTime ((toFloat i) * baseFreq) dur 0.0 {-  -}
      |> \_ -> Cmd.none {- should use Task? -}

playSines : Float -> List Int -> List (Cmd Action)
playSines baseFreq = List.map (play (Sound.currentTime ()) baseFreq)

calcurateBaseFrequency : Window.Size -> Mouse.Position -> Float
calcurateBaseFrequency w p = (1.0 - (toFloat p.y) / (toFloat w.height)) * (440.0 * 3 - 55.0) + 55.0

main : Program Never Model Action
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

init : ( Model, Cmd Action )
init = InitialPage { maybeTexture = Nothing, size = Window.Size 0 0 }
       !  [ Texture.load "homepage.png"
                |> Task.attempt
                    (\result ->
                        case result of
                            Err err ->
                                TextureError err

                            Ok val ->
                                TextureLoad val
                    )
          , Task.perform Resize Window.size
          ]


subscriptions : Model -> Sub Action
subscriptions _ =
    Sub.batch
        [ Mouse.clicks MouseClicked
        , AnimationFrame.diffs Animate
        , Window.resizes Resize
        ]


updateForInitial : Action -> ForInitial -> ( Model, Cmd Action )
updateForInitial action model =
    case action of
        Resize size ->
            InitialPage { model | size = size } ! []
        MouseClicked position ->
            let freq = calcurateBaseFrequency model.size position in
                (model.maybeTexture
                |> Maybe.map (\texture ->
                     Playing { size = model.size
                             , position = position
                             , freq = freq
                             , elapsed = 0
                             , texture = texture
                             , vols = List.repeat numWave 1.0
                             })
                |> Maybe.withDefault (InitialPage model)) ! (playSines freq (List.range 1 numWave))
        Animate elapsed ->
             InitialPage model ! []
        TextureLoad texture ->
            InitialPage { model | maybeTexture = Just texture } ! []
        TextureError _ ->
            Debug.crash "Error loading texture"

updateForPlay : Action -> ForPlay -> ( Model, Cmd Action )
updateForPlay action model =
    case action of
        Resize size ->
            Playing { model | size = size } ! []
        MouseClicked position ->
            Playing { model | position = position, freq = calcurateBaseFrequency model.size position } ! []
        Animate elapsed ->
            let newVols = model.vols
                          |> List.indexedMap (envelope (elapsed / 1000))
                cmds = newVols
                       |> List.indexedMap (\i v -> (i, v))
                       |> List.filter (\t -> Tuple.second t < 0)
                       |> List.map Tuple.first |> playSines model.freq
            in
                Playing { model
                | elapsed = elapsed + model.elapsed
                , vols = newVols |> List.map (\v -> if (v < 0) then (1.0 + v) else v)
                } ! cmds
        TextureLoad texture ->
            Debug.crash "Error texture load again?"
        TextureError _ ->
            Debug.crash "Error loading texture"

update : Action -> Model -> ( Model, Cmd Action )
update action model =
  case model of
    InitialPage model -> updateForInitial action model
    Playing model -> updateForPlay action model

viewWebGl : ForPlay -> Html Action
viewWebGl { size, texture, position, elapsed, vols} =
    case vols of
      [v1, v2, v3, v4, v5, v6, v7, v8] ->
          WebGL.toHtml
                      [ width size.width
                      , height size.height
                      , style [ ( "display", "block" ) ]
                      ]
                      [ WebGL.entityWith
                          [ Blend.add Blend.one Blend.oneMinusSrcAlpha ]
                          vertexShader
                          fragmentShader
                          mesh
                          { iResolution = vec3 (toFloat size.width) (toFloat size.height) 0
                          , iGlobalTime = elapsed / 1000
                          , texture = texture
                          , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture)))
                                               (toFloat (Tuple.second (Texture.size texture)))
                          , v1 = v1, v2 = v2, v3 = v3, v4 = v4
                          , v5 = v5, v6 = v6, v7 = v7, v8 = v8
                          }
                      ]
      _ -> Debug.crash "vols must be 8"

view : Model -> Html Action
view model =
     case model of
         Playing model -> viewWebGl model
         InitialPage _ -> Html.div [style [ ("font-size", "4em") ]]
                              [ Html.h1 [] [Html.text "This is illiichi."]
                              , Html.h3 [] [Html.text "click any place to play sound."]]

{- mesh -}

type alias Vertex =
    { position : Vec3
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 -1 1 0)
          , Vertex (vec3 1 1 0)
          , Vertex (vec3 -1 -1 0)
          )
        , ( Vertex (vec3 -1 -1 0)
          , Vertex (vec3 1 1 0)
          , Vertex (vec3 1 -1 0)
          )
        ]


{- shaders -}

type alias Uniforms =
    { iResolution : Vec3
    , iGlobalTime : Float
    , texture : WebGL.Texture
    , textureSize : Vec2
    , v1: Float, v2: Float, v3: Float, v4: Float, v5: Float, v6: Float, v7: Float, v8: Float
    }


vertexShader : Shader Vertex Uniforms { vFragCoord : Vec2 }
vertexShader =
    [glsl|

        precision mediump float;

        attribute vec3 position;
        varying vec2 vFragCoord;
        uniform vec3 iResolution;

        void main () {
            gl_Position = vec4(position, 1.0);
            vFragCoord = (position.xy + 1.0) / 2.0 * iResolution.xy;
        }

    |]

fragmentShader : WebGL.Shader {} Uniforms { vFragCoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec2      vFragCoord;
        uniform vec3      iResolution;           // viewport resolution (in pixels)
        uniform float     iGlobalTime;           // shader playback time (in seconds)
        uniform sampler2D texture;
        uniform vec2 textureSize;
        uniform float v1, v2, v3, v4, v5, v6, v7, v8;

        float fromTexture(in vec2 uv){
            if(abs(uv.x) < 1.0 && abs(uv.y) < 0.5){
                vec4 temp = texture2D(texture, vec2(uv.x * 0.5 + 0.5, uv.y + 0.5));
                float a = temp.a;
                return temp.r > 0.5 ? 0.0 : 1.0;
            }else{
                return 0.0;
            }
        }

        float v(float value){
            return 1.0 - value;
        }

        void mainImage( out vec4 fragColor, in vec2 fragCoord ) {
            vec2 uv = fragCoord.xy / iResolution.xy;
            uv = (uv * 2.0 - 1.0);

            uv.x *= iResolution.x / iResolution.y;
            float time = iGlobalTime * 0.3;

            vec3 color = vec3(
                           fromTexture(mat2(v(v5), 0.0,
                                            0.0, v(v1)) * uv) * v(v1),
                           fromTexture(mat2(v(v7), 0.0,
                                            0.0, v(v2)) * uv) * v(v2),
                           fromTexture(mat2(v(v8), 0.0,
                                            0.0, v(v4)) * uv) * v(v3));

            fragColor = vec4(1.0 - color, 1.0);
        }

        void main() {
          mainImage(gl_FragColor, vFragCoord);
        }

    |]

