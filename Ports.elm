port module Ports exposing (modalOpen, modalClose)


port modalOpen : () -> Cmd msg


port modalClose : () -> Cmd msg
