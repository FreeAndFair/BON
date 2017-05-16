module BON.Parser.AST where

data Comment = MkComment [String]
  deriving Show

data BonSourceFile
  = MkBonSourceFile [SpecificationElement] (Maybe Indexing)
  deriving Show

data SpecificationElement
  = InformalChart InformalChart
  | ClassDictionary ClassDictionary
  | StaticDiagram StaticDiagram
  | DynamicDiagram DynamicDiagram
  -- | NotationalTuning _
  deriving Show

data InformalChart
  = SystemChart ClusterChart
  | ClusterChart ClusterChart
  | ClassChart ClassChart
  | EventChart EventChart
  | ScenarioChart ScenarioChart
  | CreationChart CreationChart
  deriving Show

data ClassDictionary
  = MkClassDictionary
    String -- ^ system_name
    [DictionaryEntry] -- ^ opt(part) many1(dictionary_entry) 'end'
    (Maybe Indexing) -- ^ indexing
    (Maybe String) -- ^ explanation
    (Maybe String) -- ^ part
  deriving Show

data DictionaryEntry
  = DictionaryEntry ClassName [String] String
  deriving Show

data ClusterChart
  = MkClusterChart
    String -- ^ system/cluster name
    Bool -- ^ True for system_chart, False for cluster_chart
    [ClassEntry] -- ^ class entries (nil for system_chart)
    [ClusterEntry] -- ^ cluster entries
    (Maybe Indexing) -- ^ indexing
    (Maybe String) -- ^ explanation
    (Maybe String) -- ^ part
  deriving Show

data Indexing
  = MkIndexing [IndexClause]
  deriving Show

data ClusterEntry
  = MkClusterEntry
    String -- ^ cluster_name
    String -- ^ description
  deriving Show

data IndexClause
  = MkIndexClause
    String -- ^ identifier
    [String] -- ^ index_term_list
  deriving Show

data ClassEntry
  = MkClassEntry
    ClassName
    String -- ^ description
  deriving Show

data ClassChart
  = MkClassChart
    ClassName -- ^ class_name
    [ClassName] -- ^ inherits
    [String] -- ^ queries
    [String] -- ^ commands
    [String] -- ^ constraints
    (Maybe Indexing) -- ^ indexing
    (Maybe String) -- ^ explanation
    (Maybe String) -- ^ part
  deriving Show

type ClassName = String

data EventChart
  = MkEventChart
    String -- ^ system_name
    (Maybe Direction)
    [EventEntry]
    (Maybe Indexing)
    (Maybe String) -- ^ explanation
    (Maybe String) -- ^ part
  deriving Show

data Direction = Incoming | Outgoing
  deriving Show

data EventEntry
  = MkEventEntry
    String -- ^ 'event' description
    [String] -- ^ 'involves' class_or_cluster_name_list
  deriving Show

data ScenarioChart
  = MkScenarioChart
    String -- ^ system_name
    [ScenarioEntry]
    (Maybe Indexing)
    (Maybe String) -- ^ explanation
    (Maybe String) -- ^ part
  deriving Show

data ScenarioEntry
  = MkScenarioEntry
    String -- ^ manifest_string
    String -- ^ description
  deriving Show

data CreationChart
  = MkCreationChart
    String -- ^ system_name
    [CreationEntry]
    (Maybe Indexing)
    (Maybe String) -- ^ explanation
    (Maybe String) -- ^ part
  deriving Show

data CreationEntry
  = MkCreationEntry
    String -- ^ class_name
    [String] -- ^ class_or_cluster_name_list
  deriving Show

data StaticDiagram
  = MkStaticDiagram
    [StaticComponent]
    (Maybe String) -- ^ extended_id
    Comment
  deriving Show

data StaticComponent
  = Cluster Cluster
  | Class Class
  | StaticRelation StaticRelation
  deriving Show

data Cluster
  = MkCluster
    String -- ^ cluster_name
    [StaticComponent]
    Bool -- ^ reused?
    Comment
  deriving Show

data Class
  = MkClass
    String -- ^ class_name
    [FormalGeneric]
    (Maybe ClassMod)
    (Maybe ClassInterface)
    Bool -- ^ reused
    Bool -- ^ persistent
    Bool -- ^ interfaced
    Comment
  deriving Show

data ClassMod
  = ROOT
  | DEFERRED
  | EFFECTIVE
  deriving Show

data StaticRelation
  = InheritanceRelation InheritanceRelation
  | ClientRelation ClientRelation
  deriving Show

data InheritanceRelation
  = MkInheritanceRelation
    StaticRef -- ^ child
    StaticRef -- ^ parent
    (Maybe Integer) -- ^ multiplicity
    (Maybe String) -- ^ semantic_label
  deriving Show

data ClientRelation
  = MkClientRelation
    StaticRef -- ^ client
    StaticRef -- ^ supplier
    (Maybe ClientEntityExpression) -- ^ client_entities
    (Maybe TypeMark) -- ^ type_mark
    (Maybe String) -- ^ semantic_label
  deriving Show

data ClientEntityExpression
  = ClientEntityList [ClientEntity]
  | Multiplicity Integer
  deriving Show

data ClientEntity
  = SupplierIndirection SupplierIndirection
  | ParentIndirection ParentIndirection
  deriving Show

data SupplierIndirection
  = MkSupplierIndirection
    (Maybe IndirectionFeaturePart)
    GenericIndirection
  deriving Show

data IndirectionFeaturePart
  = FeatureName FeatureName
  | IndirectionFeatureList IndirectionFeatureList
  deriving Show

data IndirectionFeatureList
  = MkIndirectionFeatureList [FeatureName]
  deriving Show

data ParentIndirection
  = MkParentIndirection GenericIndirection
  deriving Show

data GenericIndirection
  = MkGenericIndirection IndirectionElement
  deriving Show

data NamedIndirection
  = MkNamedIndirection
    ClassName
    [IndirectionElement]
  deriving Show

data IndirectionElement
  = CompactedIndirectionElementImpl
  | NamedIndirection NamedIndirection
  | ClassName ClassName
  deriving Show

data TypeMark
  = TypeMarkHASTYPE
  | TypeMarkAGGREGATE
  | TypeMarkSHAREDMARK Integer
  deriving Show

data StaticRef
  = MkStaticRef
    [StaticRefPart] -- ^ cluster_prefix
    StaticRefPart -- ^ static_component_name
  deriving Show

data StaticRefPart
  = MkStaticRefPart String
  deriving Show

data ClassInterface
  = MkClassInterface
    [Feature] -- ^ features
    [Type] -- ^ parent_class_list
    [Expression] -- ^ class_invariant
    (Maybe Indexing) -- ^ indexing
  deriving Show

data Expression
  = Quantification Quantification
  | BinaryExp BinaryOp Expression Expression
  | UnaryExp UnaryOp Expression
  | Constant Constant
  | CallExp Expression UnqualifiedCall
  | UnqualifiedCall UnqualifiedCall
  deriving Show

data Type
  = MkType
    ClassName
    [Type] -- ^ actual_generics
  deriving Show

data Feature
  = MkFeature
    [FeatureSpecification]
    [ClassName] -- ^ selective_export
    Comment
  deriving Show

data FeatureSpecification
  = MkFeatureSpecification
    FeatureSpecificationModifier
    [FeatureName]
    [FeatureArgument]
    (Maybe ContractClause)
    (Maybe HasType)
    (Maybe RenameClause)
    Comment
  deriving Show

data FeatureSpecificationModifier
  = FeatureSpecDEFERRED
  | FeatureSpecEFFECTIVE
  | FeatureSpecREDEFINED
  | FeatureSpecNONE
  deriving Show

data HasType
  = MkHasType TypeMark Type
  deriving Show

data ContractClause
  = MkContractClause
    [Expression] -- ^ preconditions
    [Expression] -- ^ postconditions
  deriving Show

data FeatureName
  = MkFeatureName String
  deriving Show

data RenameClause
  = MkRenameClause ClassName FeatureName
  deriving Show

data FeatureArgument
  = MkFeatureArgument
    String -- ^ identifier
    Type
  deriving Show

data FormalGeneric
  = MkFormalGeneric
    String -- ^ formal_generic_name
    (Maybe Type) -- ^ class_type
  deriving Show

data Quantification
  = MkQuantification
    Quantifier
    [VariableRange] -- ^ range_expression
    (Maybe Expression) -- ^ restriction
    Expression -- ^ proposition
  deriving Show

data Quantifier = FORALL | EXISTS
  deriving Show

data VariableRange
  = MemberRange MemberRange
  | TypeRange TypeRange
  deriving Show

data MemberRange
  = MkMemberRange [String] Expression
  deriving Show

data TypeRange
  = MkTypeRange [String] Type
  deriving Show

data UnqualifiedCall
  = MkUnqualifiedCall
    String -- ^ identifier
    [Expression] -- ^ actual_arguments
  deriving Show

data EnumerationElement
  = Expression Expression
  | Interval Interval
  deriving Show

data Interval
  = IntegerInterval IntegerInterval
  | CharacterInterval CharacterInterval
  deriving Show

data IntegerInterval
  = MkIntegerInterval Integer Integer
  deriving Show

data CharacterInterval
  = MkCharacterInterval Char Char
  deriving Show

data Constant
  = ManifestConstant ManifestConstant
  | CurrentConstant
  | VoidConstant
  | ResultConstant
  deriving Show

data ManifestConstant
  = BooleanConstant Bool
  | CharacterConstant Char
  | IntegerConstant Integer
  | RealConstant Double
  | StringConstant String
  | SetConstant [EnumerationElement]
  deriving Show

data DynamicDiagram
  = MkDynamicDiagram
    [DynamicComponent] -- ^ components
    (Maybe String) -- ^ extended_id
    Comment
  deriving Show

data DynamicComponent
  = ScenarioDescription ScenarioDescription
  | ObjectGroup ObjectGroup
  | ObjectStack ObjectStack
  | ObjectInstance ObjectInstance
  | MessageRelation MessageRelation
  deriving Show

data ScenarioDescription
  = MkScenarioDescription
    String -- ^ scenario_name
    [LabelledAction] -- ^ labelled_actions
    Comment
  deriving Show

data LabelledAction
  = MkLabelledAction
    String -- ^ action_label
    String -- ^ action_description
  deriving Show

data ObjectGroup
  = MkObjectGroup
    Bool -- ^ nameless?
    String -- ^ group_name
    [DynamicComponent]
    Comment
  deriving Show

data ObjectStack
  = MkObjectStack
    ObjectName
    Comment
  deriving Show

data ObjectInstance
  = MkObjectInstance
    ObjectName
    Comment
  deriving Show

data ObjectName
  = MkObjectName
    ClassName
    (Maybe String) -- ^ extended_id
  deriving Show

data MessageRelation
  = MkMessageRelation
    ObjectName -- ^ sender
    ObjectName -- ^ receiver
    (Maybe String) -- ^ message_label
  deriving Show

data BinaryOp
  = Add | Sub
  | And | Or | Xor
  | Lt | Gt | Le | Ge | Eq | Neq
  | MemberOf | NotMemberOf
  | HasType
  | Mul | Div | IntDiv
  | Mod | Pow
  | Equiv | Implies
  deriving Show

data UnaryOp
  = UAdd | USub
  | DELTA | OLD | NOT
  deriving Show
