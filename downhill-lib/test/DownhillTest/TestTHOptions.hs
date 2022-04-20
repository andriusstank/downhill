module DownhillTest.TestTHOptions(defaultDVarOptions) where
import Downhill.TH ( mkHasGradInstances, RecordNamer(..), BVarOptions(..), AffineSpaceOptions (AutoAffineSpace), TangOptions (GenTang), GradOptions (GenGrad))

defaultTangRecordNamer :: RecordNamer
defaultTangRecordNamer =
  RecordNamer
    { typeConNamer = (++ "TangT"),
      dataConNamer = (++ "TangD"),
      fieldNamer = id
    }

defaultGradRecordNamer :: RecordNamer
defaultGradRecordNamer =
  RecordNamer
    { typeConNamer = (++ "GradT"),
      dataConNamer = (++ "GradD"),
      fieldNamer = id
    }

defaultBuilderRecordNamer :: RecordNamer
defaultBuilderRecordNamer =
  RecordNamer
    { typeConNamer = (++ "BuilderT"),
      dataConNamer = (++ "BuilderD"),
      fieldNamer = id
    }

defaultDVarOptions :: BVarOptions
defaultDVarOptions =
  BVarOptions
    { optTang = GenTang defaultTangRecordNamer,
      optGrad = GenGrad defaultGradRecordNamer,
      optBuilderNamer = defaultBuilderRecordNamer,
      optAffineSpace = AutoAffineSpace,
      optExcludeFields = []
    }

