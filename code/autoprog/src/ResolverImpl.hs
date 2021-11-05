-- Put your Resolver implementation in this file
module ResolverImpl where

import Defs

resolve :: TCEnv -> (TVName -> EM SType) -> PType -> EM SType
resolve = undefined

declare :: [TDecl] -> EM TCEnv
declare = undefined
