namespace door

  data DoorState = DoorClose | DoorOpen

  data DoorCmd : DoorState -> DoorState -> Type where
    Open : DoorCmd DoorClose DoorOpen
    Close : DoorCmd DoorOpen DoorClose
    RingBell : DoorCmd state state
    (>>=) : DoorCmd s1 s2 -> (() -> DoorCmd s2 s3) -> DoorCmd s1 s3

  doorProgress : DoorCmd DoorClose DoorClose
  doorProgress = do
    RingBell
    Open
    RingBell
    Close
