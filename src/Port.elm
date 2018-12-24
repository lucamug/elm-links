port module Port exposing
    ( historyPushState
    , onPopState
    , pageInTopArea
    )


port pageInTopArea : (Bool -> msg) -> Sub msg


port onPopState : (String -> msg) -> Sub msg


port historyPushState : String -> Cmd msg
