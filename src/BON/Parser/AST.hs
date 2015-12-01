module BON.Parser.AST where

data Comment = MkComment [String]

data BonSourceFile
  = MkBonSourceFile [SpecificationElement] (Maybe Indexing)

data SpecificationElement
  = InformalChart InformalChart
  | ClassDictionary ClassDictionary
  | StaticDiagram StaticDiagram
  | DynamicDiagram DynamicDiagram
  -- | NotationalTuning _

data InformalChart
  = SystemChart ClusterChart
  | ClusterChart ClusterChart
  | ClassChart ClassChart
  | EventChart EventChart
  | ScenarioChart ScenarioChart
  | CreationChart CreationChart

data ClassDictionary
  = MkClassDictionary
    String -- ^ system_name
    [DictionaryEntry] -- ^ opt(part) many1(dictionary_entry) 'end'
    (Maybe Indexing) -- ^ indexing
    (Maybe String) -- ^ explanation
    (Maybe String) -- ^ part

data DictionaryEntry
  = DictionaryEntry ClassName [String] String

data ClusterChart
  = MkClusterChart
    String -- ^ system/cluster name
    Bool -- ^ True for system_chart, False for cluster_chart
    [ClassEntry] -- ^ class entries (nil for system_chart)
    [ClusterEntry] -- ^ cluster entries
    (Maybe Indexing) -- ^ indexing
    (Maybe String) -- ^ explanation
    (Maybe String) -- ^ part

data Indexing
  = MkIndexing [IndexClause]

data ClusterEntry
  = MkClusterEntry
    String -- ^ cluster_name
    String -- ^ description

data IndexClause
  = MkIndexClause
    String -- ^ identifier
    [String] -- ^ index_term_list

data ClassEntry
  = MkClassEntry
    ClassName
    String -- ^ description

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

type ClassName = String

data EventChart
  = MkEventChart
    String -- ^ system_name
    (Maybe Direction)
    [EventEntry]
    (Maybe Indexing)
    (Maybe String) -- ^ explanation
    (Maybe String) -- ^ part

data Direction = Incoming | Outgoing

data EventEntry
  = MkEventEntry
    String -- ^ 'event' description
    [String] -- ^ 'involves' class_or_cluster_name_list

data ScenarioChart
  = MkScenarioChart
    String -- ^ system_name
    [ScenarioEntry]
    (Maybe Indexing)
    (Maybe String) -- ^ explanation
    (Maybe String) -- ^ part

data ScenarioEntry
  = MkScenarioEntry
    String -- ^ manifest_string
    String -- ^ description

data CreationChart
  = MkCreationChart
    String -- ^ system_name
    [CreationEntry]
    (Maybe Indexing)
    (Maybe String) -- ^ explanation
    (Maybe String) -- ^ part

data CreationEntry
  = MkCreationEntry
    String -- ^ class_name
    [String] -- ^ class_or_cluster_name_list

data StaticDiagram
  = MkStaticDiagram
    [StaticComponent]
    (Maybe String) -- ^ extended_id
    Comment

data StaticComponent
  = Cluster Cluster
  | Class Class
  | StaticRelation StaticRelation

data Cluster
  = MkCluster
    String -- ^ cluster_name
    [StaticComponent]
    Bool -- ^ reused?
    Comment

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

data ClassMod
  = ROOT
  | DEFERRED
  | EFFECTIVE

data StaticRelation
  = InheritanceRelation InheritanceRelation
  | ClientRelation ClientRelation

data InheritanceRelation
  = MkInheritanceRelation
    StaticRef -- ^ child
    StaticRef -- ^ parent
    (Maybe Integer) -- ^ multiplicity
    (Maybe String) -- ^ semantic_label

data ClientRelation
  = MkClientRelation
    StaticRef -- ^ client
    StaticRef -- ^ supplier
    (Maybe ClientEntityExpression) -- ^ client_entities
    (Maybe TypeMark) -- ^ type_mark
    (Maybe String) -- ^ semantic_label

data ClientEntityExpression
  = ClientEntityList [ClientEntity]
  | Multiplicity Integer

data ClientEntity
  = SupplierIndirection SupplierIndirection
  | ParentIndirection ParentIndirection

data SupplierIndirection
  = MkSupplierIndirection
    (Maybe IndirectionFeaturePart)
    GenericIndirection

data IndirectionFeaturePart
  = FeatureName FeatureName
  | IndirectionFeatureList IndirectionFeatureList

data IndirectionFeatureList
  = MkIndirectionFeatureList [FeatureName]

data ParentIndirection
  = MkParentIndirection GenericIndirection

data GenericIndirection
  = MkGenericIndirection IndirectionElement

data NamedIndirection
  = MkNamedIndirection
    ClassName
    [IndirectionElement]

data IndirectionElement
  = CompactedIndirectionElementImpl
  | NamedIndirection NamedIndirection
  | ClassName ClassName

data TypeMark
  = TypeMarkHASTYPE
  | TypeMarkAGGREGATE
  | TypeMarkSHAREDMARK Integer

data StaticRef
  = MkStaticRef
    [StaticRefPart] -- ^ cluster_prefix
    StaticRefPart -- ^ static_component_name

data StaticRefPart
  = MkStaticRefPart String

data ClassInterface
  = MkClassInterface
    [Feature] -- ^ features
    [Type] -- ^ parent_class_list
    [Expression] -- ^ class_invariant
    (Maybe Indexing) -- ^ indexing

data Expression
  = Quantification Quantification
  | BinaryExp BinaryOp Expression Expression
  | UnaryExp UnaryOp Expression
  | Constant Constant
  | CallExp Expression UnqualifiedCall
  | UnqualifiedCall UnqualifiedCall

data Type
  = MkType
    ClassName
    [Type] -- ^ actual_generics

data Feature
  = MkFeature
    [FeatureSpecification]
    [ClassName] -- ^ selective_export
    Comment

data FeatureSpecification
  = MkFeatureSpecification
    FeatureSpecificationModifier
    [FeatureName]
    [FeatureArgument]
    (Maybe ContractClause)
    (Maybe HasType)
    (Maybe RenameClause)
    Comment

data FeatureSpecificationModifier
  = FeatureSpecDEFERRED
  | FeatureSpecEFFECTIVE
  | FeatureSpecREDEFINED
  | FeatureSpecNONE

data HasType
  = MkHasType TypeMark Type

data ContractClause
  = MkContractClause
    [Expression] -- ^ preconditions
    [Expression] -- ^ postconditions

data FeatureName
  = MkFeatureName String

data RenameClause
  = MkRenameClause ClassName FeatureName

data FeatureArgument
  = MkFeatureArgument
    String -- ^ identifier
    Type

data FormalGeneric
  = MkFormalGeneric
    String -- ^ formal_generic_name
    (Maybe Type) -- ^ class_type

data Quantification
  = MkQuantification
    Quantifier
    [VariableRange] -- ^ range_expression
    (Maybe Expression) -- ^ restriction
    Expression -- ^ proposition

data Quantifier = FORALL | EXISTS

data VariableRange
  = MemberRange MemberRange
  | TypeRange TypeRange

data MemberRange
  = MkMemberRange [String] Expression

data TypeRange
  = MkTypeRange [String] Type

data UnqualifiedCall
  = MkUnqualifiedCall
    String -- ^ identifier
    [Expression] -- ^ actual_arguments

data EnumerationElement
  = Expression Expression
  | Interval Interval

data Interval
  = IntegerInterval IntegerInterval
  | CharacterInterval CharacterInterval

data IntegerInterval
  = MkIntegerInterval Integer Integer

data CharacterInterval
  = MkCharacterInterval Char Char

data Constant
  = ManifestConstant ManifestConstant
  | CurrentConstant
  | VoidConstant
  | ResultConstant

data ManifestConstant
  = BooleanConstant Bool
  | CharacterConstant Char
  | IntegerConstant Integer
  | RealConstant Double
  | StringConstant String
  | SetConstant [EnumerationElement]

data DynamicDiagram
  = MkDynamicDiagram
    [DynamicComponent] -- ^ components
    (Maybe String) -- ^ extended_id
    Comment

data DynamicComponent
  = ScenarioDescription ScenarioDescription
  | ObjectGroup ObjectGroup
  | ObjectStack ObjectStack
  | ObjectInstance ObjectInstance
    -- TODO: message_relation

data ScenarioDescription
  = MkScenarioDescription
    String -- ^ scenario_name
    [LabelledAction] -- ^ labelled_actions
    Comment

data LabelledAction
  = MkLabelledAction
    String -- ^ action_label
    String -- ^ action_description

data ObjectGroup
  = MkObjectGroup
    Bool -- ^ nameless?
    String -- ^ group_name
    [DynamicComponent]
    Comment

data ObjectStack
  = MkObjectStack
    ObjectName
    Comment

data ObjectInstance
  = MkObjectInstance
    ObjectName
    Comment

data ObjectName
  = MkObjectName
    ClassName
    (Maybe String) -- ^ extended_id

data BinaryOp
  = Add | Sub
  | And | Or | Xor
  | Lt | Gt | Le | Ge | Eq | Neq
  | MemberOf | NotMemberOf
  | HasType
  | Mul | Div | IntDiv
  | Mod | Pow
  | Equiv | Implies

data UnaryOp
  = UAdd | USub
  | DELTA | OLD | NOT
