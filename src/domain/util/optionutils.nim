import options

func optString*(v: Option): string {.noSideEffect.} = return if not v.isNone(): $v.get() else: ""
