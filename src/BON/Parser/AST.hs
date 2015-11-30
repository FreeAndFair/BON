module BON.Parser.AST where

import BON.Parser.Position

type LString = Located String

data BonSourceFile
  = MkBonSourceFile [SpecificationElement] (Maybe Indexing) Range

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
  = DictionaryEntry ClassName [String] String Range

data ClusterChart
  = MkClusterChart
    String -- ^ system/cluster name
    Bool -- ^ True for system_chart, False for cluster_chart
    [ClassEntry] -- ^ class entries (nil for system_chart)
    [ClusterEntry] -- ^ cluster entries
    (Maybe Indexing) -- ^ indexing
    (Maybe LString) -- ^ explanation
    (Maybe LString) -- ^ part
    Range

data Indexing
  = MkIndexing [IndexClause] Range

data ClusterEntry
  = MkClusterEntry
    String -- ^ cluster_name
    String -- ^ description
    Range

data IndexClause
  = MkIndexClause
    LString -- ^ identifier
    (Located [String]) -- ^ index_term_list
    Range

data ClassEntry
  = MkClassEntry
    ClassName
    LString -- ^ description
    Range

data ClassChart
  = MkClassChart
    ClassName -- ^ class_name
    [ClassName] -- ^ inherits
    [LString] -- ^ queries
    [LString] -- ^ commands
    [LString] -- ^ constraints
    (Maybe Indexing) -- ^ indexing
    (Maybe LString) -- ^ explanation
    (Maybe LString) -- ^ part
    Range

type ClassName = Located String

data EventChart
  = MkEventChart
    String -- ^ system_name
    (Maybe Direction)
    [EventEntry]
    (Maybe Indexing)
    (Maybe String) -- ^ explanation
    (Maybe String) -- ^ part
    Range

data Direction = Incoming | Outgoing

data EventEntry
  = MkEventEntry
    String -- ^ description

data ScenarioChart
  = MkScenarioChart
    String -- ^ system_name
    [ScenarioEntry]
    (Maybe Indexing)

data ScenarioEntry
  = MkScenarioEntry
    String -- ^ manifest_string
    String -- ^ description
    Range

data CreationChart
  = MkCreationChart
    String -- ^ system_name
    [CreationEntry]
    (Maybe Indexing)

data CreationEntry
  = MkCreationEntry
    String -- ^ class_name
    [String] -- ^ class_or_cluster_name_list
    Range

data StaticDiagram
  = MkStaticDiagram
    [StaticComponent]
    (Maybe LString) -- ^ extended_id
    String -- ^ comment
    Range

data StaticComponent
  = Cluster Cluster
  | Class Class
  | StaticRelation StaticRelation

data Cluster
  = MkCluster
    String -- ^ cluster_name
    [StaticComponent]
    Bool -- ^ reused?
    String -- ^ comment
    Range

data Class
  = MkClass
    String -- ^ class_name
    [FormalGeneric]
    (Maybe ClassMod)
    (Maybe ClassInterface)
    Bool -- ^ reused
    Bool -- ^ persistent
    Bool -- ^ interfaced
    String -- ^ comment
    Range

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
  = ClientEntityList [ClientEntity] Range
  | Multiplicity (Located Integer)

data ClientEntity
  = SupplierIndirection SupplierIndirection
  | ParentIndirection ParentIndirection

data SupplierIndirection
  = MkSupplierIndirection
    (Maybe IndirectionFeaturePart)

data IndirectionFeaturePart
  = FeatureName FeatureName
  | IndirectionFeatureList IndirectionFeatureList

data IndirectionFeatureList
  = MkIndirectionFeatureList [FeatureName] Range

data ParentIndirection
  = MkParentIndirection GenericIndirection Range

data GenericIndirection
  = MkGenericIndirection IndirectionElement Range

data NamedIndirection
  = MkNamedIndirection
    ClassName
    [IndirectionElement]
    Range

data IndirectionElement
  = CompactedIndirectionElementImpl Range
  | NamedIndirection NamedIndirection
  | ClassName ClassName

data TypeMark
  = TypeMarkHASTYPE Range
  | TypeMarkAGGREGATE Range
  | TypeMarkSHAREDMARK Integer Range

data StaticRef
  = MkStaticRef
    [StaticRefPart] -- ^ cluster_prefix
    StaticRefPart -- ^ static_component_name
    Range

data StaticRefPart
  = MkStaticRefPart (Located String)

data ClassInterface
  = MkClassInterface
    [Feature] -- ^ features
    [Type] -- ^ parent_class_list
    [Expression] -- ^ class_invariant
    (Maybe Indexing) -- ^ indexing
    Range

data Expression
  = Quantification Quantification
  | BinaryExp BinaryOp Expression Expression (Maybe Range)
  | UnaryExp UnaryOp Expression (Maybe Range)
  | Constant Constant
  | CallExp Expression UnqualifiedCall
  | UnqualifiedCall UnqualifiedCall

data Type
  = MkType
    ClassName
    [Type] -- ^ actual_generics
    Range

data Feature
  = MkFeature
    [FeatureSpecification]
    [ClassName] -- ^ selective_export

data FeatureSpecification
  = MkFeatureSpecification
    (Maybe FeatureSpecificationModifier)
    [FeatureName]
    [FeatureArgument]
    (Maybe ContractClause)
    (Maybe HasType)
    (Maybe RenameClause)

data FeatureSpecificationModifier
  = FeatureSpecDEFERRED
  | FeatureSpecEFFECTIVE
  | FeatureSpecREDEFINED
  | FeatureSpecNONE

data HasType
  = MkHasType TypeMark Type Range

data ContractClause
  = MkContractClause
    [Expression] -- ^ preconditions
    [Expression] -- ^ postconditions
    Range

data FeatureName
  = MkFeatureName String Range

data RenameClause
  = MkRenameClause ClassName FeatureName Range

data FeatureArgument
  = MkFeatureArgument
    String -- ^ identifier
    Type
    Range

data FormalGeneric
  = MkFormalGeneric
    String -- ^ formal_generic_name
    (Maybe Type) -- ^ class_type
    Range

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
  = MkMemberRange [String] Expression Range

data TypeRange
  = MkTypeRange [String] Type Range

data UnqualifiedCall
  = MkUnqualifiedCall
    String -- ^ identifier
    [Expression] -- ^ actual_arguments
    Range

data EnumerationElement
  = Expression Expression
  | Interval Interval

data Interval
  = IntegerInterval IntegerInterval
  | CharacterInterval CharacterInterval

data IntegerInterval
  = MkIntegerInterval Integer Integer Range

data CharacterInterval
  = MkCharacterInterval Char Char Range

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
    String -- ^ comment
    Range

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
    (Maybe String) -- ^ comment
    Range

data LabelledAction
  = MkLabelledAction
    String -- ^ action_label
    String -- ^ action_description
    Range

data ObjectGroup
  = MkObjectGroup
    Bool -- ^ nameless?
    String -- ^ group_name
    [DynamicComponent]
    String -- ^ comment
    Range

data ObjectStack
  = MkObjectStack
    ObjectName
    String -- ^ comment
    Range

data ObjectInstance
  = MkObjectInstance
    ObjectName
    String -- ^ comment
    Range

data ObjectName
  = MkObjectName
    ClassName
    (Maybe String) -- ^ extended_id
    Range

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
