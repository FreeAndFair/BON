{
{- Adapted from Fintan Fairmichael's BON.g. -}

module BON.Parser where

import BON.Parser.AST
import BON.Parser.Lexer

import Data.Maybe (fromMaybe)

}

%token
  EOF                { TEnd }
  IDENTIFIER         { TIdent $$ }
  INTEGER            { TNat $$ }
  CHARACTER_CONSTANT { $$ }
  MANIFEST_STRING    { TString $$ }
  REAL               { TReal $$ }
  COMMENT            { TComment $$ }
  manifest_textblock { TString $$ }

  'Current'          { TKey "Current" }
  'Void'             { TKey "Void" }
  'Result'           { TKey "Result" }
  'action'           { TKey "action" }
  'and'              { TKey "and" }
  'class'            { TKey "class" }
  'class_chart'      { TKey "class_chart" }
  'client'           { TKey "client" }
  'cluster'          { TKey "cluster" }
  'cluster_chart'    { TKey "cluster_chart" }
  'command'          { TKey "command" }
  'component'        { TKey "component" }
  'constraint'       { TKey "constraint" }
  'creates'          { TKey "creates" }
  'creation_chart'   { TKey "creation_chart" }
  'creator'          { TKey "creator" }
  'deferred'         { TKey "deferred" }
  'delta'            { TKey "delta" }
  'description'      { TKey "description" }
  'dictionary'       { TKey "dictionary" }
  'dynamic_diagram'  { TKey "dynamic_diagram" }
  'effective'        { TKey "effective" }
  'end'              { TKey "end" }
  'ensure'           { TKey "ensure" }
  'event'            { TKey "event" }
  'event_chart'      { TKey "event_chart" }
  'exists'           { TKey "exists" }
  'explanation'      { TKey "explanation" }
  'false'            { TKey "false" }
  'feature'          { TKey "feature" }
  'for_all'          { TKey "for_all" }
  'incoming'         { TKey "incoming" }
  'indexing'         { TKey "indexing" }
  'inherit'          { TKey "inherit" }
  'interfaced'       { TKey "interfaced" }
  'invariant'        { TKey "invariant" }
  'involves'         { TKey "involves" }
  'it_holds'         { TKey "it_holds" }
  'member_of'        { TKey "member_of" }
  'nameless'         { TKey "nameless" }
  'not'              { TKey "not" }
  'object'           { TKey "object" }
  'object_group'     { TKey "object_group" }
  'object_stack'     { TKey "object_stack" }
  'old'              { TKey "old" }
  'or'               { TKey "or" }
  'outgoing'         { TKey "outgoing" }
  'part'             { TKey "part" }
  'persistent'       { TKey "persistent" }
  'query'            { TKey "query" }
  'redefined'        { TKey "redefined" }
  'require'          { TKey "require" }
  'reused'           { TKey "reused" }
  'root'             { TKey "root" }
  'scenario'         { TKey "scenario" }
  'scenario_chart'   { TKey "scenario_chart" }
  'static_diagram'   { TKey "static_diagram" }
  'such_that'        { TKey "such_that" }
  'system_chart'     { TKey "system_chart" }
  'true'             { TKey "true" }
  'xor'              { TKey "xor" }
  ';'                { TKey ";" }
  ':'                { TKey ":" }
  ','                { TKey "," }
  '.'                { TKey "." }
  '('                { TKey "(" }
  ')'                { TKey ")" }
  '['                { TKey "[" }
  ']'                { TKey "]" }
  '{'                { TKey "{" }
  '}'                { TKey " }" }
  ':{'               { TKey ":{" }
  '<->'              { TKey "<->" }
  '->'               { TKey "->" }
  '<-'               { TKey "<-" }
  '<'                { TKey "<" }
  '>'                { TKey ">" }
  '<='               { TKey "<=" }
  '>='               { TKey ">=" }
  '='                { TKey "=" }
  '/='               { TKey "/=" }
  '+'                { TKey "+" }
  '-'                { TKey "-" }
  '*'                { TKey "*" }
  '/'                { TKey "/" }
  '//'               { TKey "//" }
  '\\\\'             { TKey "\\\\" }
  '^'                { TKey "^" }
  '..'               { TKey ".." }
  '...'              { TKey "..." }

%name prog prog
%tokentype { Token }

%left '<->'
%right '->'
%left 'and' 'or' 'xor'
%left '<' '>' '<=' '>=' '=' '/=' 'member_of' ':'
%left '+' '-'
%left '*' '/' '//'
%right '\\\\' '^'
%right 'delta' 'old' 'not' NEG PLUS
%%

{-
grammar BON;
options {
  ASTLabelType=CommonTree;
  superClass=AbstractBONParser;
}

@header {
  package ie.ucd.bon.parser;

  import ie.ucd.bon.parser.errors.MissingElementParseError;
  import ie.ucd.bon.ast.*;
  import java.io.File;
}

@lexer::header {
/**
 * Copyright (c) 2007, Fintan Fairmichael, University College Dublin under the BSD licence.
 * See LICENCE.TXT for details.
 */
package ie.ucd.bon.parser;
}
-}

opt(p)
  : p           { Just $1 }
  | {- empty -} { Nothing }

rev_list(p)
  : rev_list(p) p  { $2 : $1 }
  | {- empty -}    { [] }

-- List of zero or more p.
list(p)
  : rev_list(p)    { reverse $1 }

rev_list1(p)
  : rev_list1(p) p { $2 : $1 }
  | p              { [$1] }

-- List of one or more p.
list1(p)
  : rev_list1(p)   { reverse $1 }

snd(p, q) : p q { $2 }

-- List of one or more p, separated by q.
sep1(p, q)
  : p list(snd(q,p)) { $1 : $2 }

-- List of one or more p, separated by q, with optional final q.
sep1f(p, q)
  : p sepg(p, q) { $1 : $2 }

sepg(p, q)
  : q p sepg(p, q) { $2 : $3 }
  | q              { [] }
  | {- empty -}    { [] }

{-
 ##############################################
 ###   Parser...                            ###
 ##############################################
-}

prog :: { BonSourceFile }
  : bon_specification EOF
      { MkBonSourceFile $1 Nothing }
  | indexing bon_specification EOF
      { MkBonSourceFile $2 (Just $1) }
  | EOF
      -- { addParseProblem(MissingElementParseError(getLoc $1, "at least one specification entry", "in source file", True)) }
      { MkBonSourceFile [] Nothing }
  | indexing EOF
      -- { addParseProblem(MissingElementParseError(getLoc ($2), "at least one specification entry", "in source file", True)) }
      { MkBonSourceFile [] (Just $1) }

{-
/**********************************************
 ***   BON Specification                    ***
 **********************************************/
-}

bon_specification :: { [SpecificationElement] }
  : list1(specification_element) { $1 }

specification_element :: { SpecificationElement }
  : informal_chart   { InformalChart $1 }
  | class_dictionary { ClassDictionary $1 }
  | static_diagram   { StaticDiagram $1 }
  | dynamic_diagram  { DynamicDiagram $1 }
  -- | notational_tuning { NotationalTuning $1 }

{-
/**********************************************
 ***   Informal charts                      ***
 **********************************************/
-}

informal_chart :: { InformalChart }
  : system_chart    { SystemChart $1 }
  | cluster_chart   { ClusterChart $1 }
  | class_chart     { ClassChart $1 }
  | event_chart     { EventChart $1 }
  | scenario_chart  { ScenarioChart $1 }
  | creation_chart  { CreationChart $1 }

class_dictionary :: { ClassDictionary }
  : 'dictionary' system_name opt(indexing) opt(explanation) opt(part) list1(dictionary_entry) 'end'
  { MkClassDictionary $2 $6 $3 $4 $5 }

dictionary_entry :: { DictionaryEntry }
  : 'class' class_name 'cluster' sep1(cluster_name, ',') description
  { DictionaryEntry $2 $4 $5 }

------------------------------------------------

system_chart :: { ClusterChart }
  : 'system_chart'
    system_name
    opt(indexing)
    opt(explanation)
    opt(part)
    list(cluster_entry)
    'end'
  { MkClusterChart $2 True [] $6 $3 $4 $5 }

explanation :: { String }
  : 'explanation' manifest_textblock { $2 }
{-
  | 'explanation'
  { addParseProblem(MissingElementParseError(getLoc $1, "explanation text", "after 'explanation'", False)) }
-}

indexing :: { Indexing }
  : 'indexing' index_list { MkIndexing $2 }
  | 'indexing'
  --{ addParseProblem(MissingElementParseError(getLoc ($1), "indexing entries", "after 'indexing'", False)) }
  { MkIndexing [] }

part :: { String }
  : 'part' MANIFEST_STRING { $2 }
  | 'part'
    --{ addParseProblem(MissingElementParseError(getLoc ($p), "part text", "after 'part'", false)); }
    { "" }

description :: { String }
  : 'description' manifest_textblock { $2 }

cluster_entry :: { ClusterEntry }
  : 'cluster' cluster_name description
  { MkClusterEntry $2 $3 }

system_name :: { String }
  : IDENTIFIER { $1 }

------------------------------------------------

index_list :: { [IndexClause] }
  : sep1f(index_clause, ';') { $1 }

index_clause :: { IndexClause }
  : IDENTIFIER ':' index_term_list
  { MkIndexClause $1 $3 }
{-
  | IDENTIFIER ':'
  { addParseProblem(MissingElementParseError(getLoc ($1), "index term(s)", "in index clause", True)) }
-}

index_term_list :: { [String] }
  : sep1(index_string, ',') { $1 }

index_string :: { String }
  : manifest_textblock { $1 }


------------------------------------------------

cluster_chart :: { ClusterChart }
  : 'cluster_chart'
    cluster_name
    opt(indexing)
    opt(explanation)
    opt(part)
    list(class_entry)
    list(cluster_entry)
    'end'
  { MkClusterChart $2 False $6 $7 $3 $4 $5 }

class_entry :: { ClassEntry }
  : 'class' class_name description { MkClassEntry $2 $3 }

cluster_name :: { String }
  : IDENTIFIER { $1 }

------------------------------------------------

class_chart :: { ClassChart }
  : 'class_chart'
    class_name
    opt(indexing)
    opt(explanation)
    opt(part)
    opt(inherits)
    opt(queries)
    opt(commands)
    opt(constraints)
    'end'
  { MkClassChart $2 (fromMaybe [] $6) (fromMaybe [] $7) (fromMaybe [] $8) (fromMaybe [] $9) $3 $4 $5 }

inherits :: { [ClassName] }
  : 'inherit' class_name_list { $2 }
{-
  | 'inherit'
  { addParseProblem(MissingElementParseError(getLoc ($1), "class name(s)", "in inherits clause", True)) }
-}

queries :: { [String] }
  : 'query' query_list { $2 }

commands :: { [String] }
  : 'command' command_list { $2 }

constraints :: { [String] }
  : 'constraint' constraint_list { $2 }

query_list :: { [String] }
  : sep1f(manifest_textblock, ',') { $1 }

command_list :: { [String] }
  : sep1f(manifest_textblock, ',') { $1 }

constraint_list :: { [String] }
  : sep1f(manifest_textblock, ',') { $1 }

class_name_list :: { [ClassName] }
  : sep1(class_name, ',') { $1 }

class_or_cluster_name_list :: { [String] }
  : sep1(class_or_bracketed_cluster_name, ',') { $1 }

class_or_bracketed_cluster_name :: { String }
  : class_name { $1 }
  | '(' cluster_name ')' { $2 }

class_name :: { ClassName }
  : IDENTIFIER { $1 }

------------------------------------------------

event_chart :: { EventChart }
  : 'event_chart'
     system_name
     opt(direction)
     opt(indexing)
     opt(explanation)
     opt(part)
     list(event_entry)
     'end'
  { MkEventChart $2 $3 $7 $4 $5 $6 }

direction :: { Direction }
  : 'incoming' { Incoming }
  | 'outgoing' { Outgoing }

event_entry :: { EventEntry }
  : 'event'
    manifest_textblock
    'involves'
    class_or_cluster_name_list
  { MkEventEntry $2 $4 }

------------------------------------------------

scenario_chart :: { ScenarioChart }
  : 'scenario_chart'
    system_name
    opt(indexing)
    opt(explanation)
    opt(part)
    list(scenario_entry)
    'end'
  { MkScenarioChart $2 $6 $3 $4 $5 }

scenario_entry :: { ScenarioEntry }
  : 'scenario' MANIFEST_STRING description
  { MkScenarioEntry $2 $3 }

------------------------------------------------

creation_chart :: { CreationChart }
  : 'creation_chart'
    system_name
    opt(indexing)
    opt(explanation)
    opt(part)
    list(creation_entry)
    'end'
  { MkCreationChart $2 $6 $3 $4 $5 }

creation_entry :: { CreationEntry }
  : 'creator' class_name 'creates' class_or_cluster_name_list
  { MkCreationEntry $2 $4 }

{-
/**********************************************
 ***   Static Diagrams                      ***
 **********************************************/
-}

static_diagram :: { StaticDiagram }
  : 'static_diagram' opt(extended_id) comment 'component' list(static_component) 'end'
  { MkStaticDiagram $5 $2 $3 }

extended_id :: { String }
  : IDENTIFIER { $1 }
  | INTEGER { show $1 }

static_component :: { StaticComponent }
  : cluster         { Cluster $1 }
  | class           { Class $1 }
  | static_relation { StaticRelation $1 }

------------------------------------------------

cluster :: { Cluster }
  : 'cluster'
    cluster_name
    reused
    comment
    opt(cluster_components)
  { MkCluster $2 (fromMaybe [] $5) $3 $4 }

reused :: { Bool }
  : 'reused'    { True }
  | {- empty -} { False }

cluster_components :: { [StaticComponent] }
  : 'component' list(static_component) 'end' { $2 }

class :: { Class }
  : opt(class_mod)
    'class'
    class_name
    opt(formal_generics)
    reused
    persistent
    interfaced
    comment
    opt(class_interface)
  { MkClass $3 (fromMaybe [] $4) $1 $9 $5 $6 $7 $8 }

class_mod :: { ClassMod }
  : 'root'      { ROOT }
  | 'deferred'  { DEFERRED }
  | 'effective' { EFFECTIVE }

persistent :: { Bool }
  : 'persistent' { True }
  | {- empty -}  { False }

interfaced :: { Bool }
  : 'interfaced' { True }
  | {- empty -}  { False }

comment :: { Comment }
  : list1(COMMENT) { MkComment $1 }

static_relation :: { StaticRelation }
  : inheritance_relation { InheritanceRelation $1 }
  | client_relation      { ClientRelation $1 }

------------------------------------------------

inheritance_relation :: { InheritanceRelation }
  : child
    'inherit'
    opt(multiplicity_braces)
    parent
    opt(semantic_label)
  { MkInheritanceRelation $1 $4 $3 $5 }

multiplicity_braces :: { Integer }
  : '{' multiplicity '}' { $2 }

client_relation :: { ClientRelation }
  : client
    'client'
    opt(client_entities)
    opt(type_mark)
    supplier
    opt(semantic_label)
  { MkClientRelation $1 $5 $3 $4 $6 }

client_entities :: { ClientEntityExpression }
  : '{' client_entity_expression '}' { $2 }

client_entity_expression :: { ClientEntityExpression }
  : client_entity_list { ClientEntityList $1 }
  | multiplicity { Multiplicity $1 }

client_entity_list :: { [ClientEntity] }
  : sep1(client_entity, ',') { $1 }

--Conflict here is:
-- feature_name can be an IDENTIFIER, and supplier_indirection can also be an IDENTIFIER
--TODO
--client_entity  :    feature_name
client_entity :: { ClientEntity }
  : {- prefix FIXME
  | infix
  | -} supplier_indirection { SupplierIndirection $1 }
  | parent_indirection { ParentIndirection $1 }

supplier_indirection :: { SupplierIndirection }
  : indirection_feature_part ':' generic_indirection
    { MkSupplierIndirection (Just $1) $3 }
  | generic_indirection
    { MkSupplierIndirection Nothing $1 }

indirection_feature_part :: { IndirectionFeaturePart }
  : feature_name { FeatureName $1 }
  | indirection_feature_list { IndirectionFeatureList $1 }

indirection_feature_list :: { IndirectionFeatureList }
  : '(' feature_name_list ')'
  { MkIndirectionFeatureList $2 }

parent_indirection :: { ParentIndirection }
  : '->' generic_indirection
  { MkParentIndirection $2 }

------------------------------------------------

generic_indirection :: { GenericIndirection }
--  formal_generic_name
                       --NB - changed the below... both are IDENTIFIERs
-- |
  : indirection_element
  { MkGenericIndirection $1 }

named_indirection :: { NamedIndirection }
  : class_name '[' indirection_list ']'
   { MkNamedIndirection $1 $3 }

indirection_list :: { [IndirectionElement] }
  : sep1(indirection_element, ',') { $1 }

indirection_element :: { IndirectionElement }
  : '...' { CompactedIndirectionElementImpl }
  | named_indirection { NamedIndirection $1 }
  | class_name { ClassName $1 }

type_mark :: { TypeMark }
  : ':' { TypeMarkHASTYPE }
  | ':{' { TypeMarkAGGREGATE }
  | shared_mark { $1 }

shared_mark :: { TypeMark }
  : ':' '(' multiplicity ')'
  { TypeMarkSHAREDMARK $3 }

------------------------------------------------

child :: { StaticRef }
  : static_ref { $1 }

parent :: { StaticRef }
  : static_ref { $1 }

client :: { StaticRef }
  : static_ref { $1 }

supplier :: { StaticRef }
  : static_ref { $1 }

static_ref :: { StaticRef }
  : sep1(static_component_name,'.') { MkStaticRef (init $1) (last $1) }

--TODO - class_name and cluster_name are both just IDENTIFIERs.
--static_component_name  :  class_name | cluster_name
static_component_name :: { StaticRefPart }
  : IDENTIFIER { MkStaticRefPart $1 }

multiplicity :: { Integer }
  : INTEGER { $1 }

semantic_label :: { String }
  : MANIFEST_STRING { $1 }

{-
/**********************************************
 ***   Class Interface Description          ***
 **********************************************/
-}

class_interface :: { ClassInterface }
  : class_interface_start_indexing { $1 }
  | class_interface_start_inherit { $1 }
  | class_interface_start_features { $1 }
  | class_interface_start_invariant { $1 }

class_interface_start_indexing :: { ClassInterface }
  : indexing
    opt(parent_class_list)
    list(feature_clause)
    opt(class_invariant)
    'end'
  { MkClassInterface $3 (fromMaybe [] $2) (fromMaybe [] $4) (Just $1) }

class_interface_start_inherit :: { ClassInterface }
  : parent_class_list
    list(feature_clause)
    opt(class_invariant)
    'end'
  { MkClassInterface $2 $1 (fromMaybe [] $3) Nothing }

class_interface_start_features :: { ClassInterface }
  : list1(feature_clause)
    opt(class_invariant)
    'end'
  { MkClassInterface $1 [] (fromMaybe [] $2) Nothing }

class_interface_start_invariant :: { ClassInterface }
  : class_invariant
    'end'
  { MkClassInterface [] [] $1 Nothing }

class_invariant :: { [Expression] }
  : 'invariant' assertion { $2 }

parent_class_list :: { [Type] }
  : 'inherit' sep1f(class_type, ';') { $2 }
{-
|
  i='inherit'
  { addParseProblem(new MissingElementParseError(getLoc ($i), "class name(s)", "in inherits clause", true)); }
-}

------------------------------------------------

feature_clause :: { Feature }
  : 'feature'
    opt(selective_export)
    comment
    list1(feature_specification)
  { MkFeature $4 (fromMaybe [] $2) $3 }

feature_specification :: { FeatureSpecification }
  : feature_specification_modifier
    feature_name_list
    opt(has_type)
    opt(rename_clause)
    comment
    list(feature_argument)
    opt(contract_clause)
  { MkFeatureSpecification $1 $2 (concat $6) $7 $3 $4 $5 }

feature_specification_modifier :: { FeatureSpecificationModifier }
  : 'deferred'  { FeatureSpecDEFERRED }
  | 'effective' { FeatureSpecEFFECTIVE }
  | 'redefined' { FeatureSpecREDEFINED }
  | {- empty -} { FeatureSpecNONE }

has_type :: { HasType }
  : type_mark type { MkHasType $1 $2 }
  | type_mark 'Void' { MkHasType $1 (MkType "Void" []) }

------------------------------------------------

contract_clause :: { ContractClause }
  : contracting_conditions 'end' { $1 }

--NB. Rewritten from precondition | postcondition | pre_and_post
contracting_conditions :: { ContractClause }
  : precondition { MkContractClause $1 [] }
  | postcondition { MkContractClause [] $1 }
  | precondition postcondition  { MkContractClause $1 $2 }

precondition :: { [Expression] }
  : 'require' assertion { $2 }

postcondition :: { [Expression] }
  : 'ensure' assertion { $2 }

------------------------------------------------

selective_export :: { [ClassName] }
  : '{' class_name_list '}' { $2 }

feature_name_list :: { [FeatureName] }
  : sep1(feature_name, ',') { $1 }

feature_name :: { FeatureName }
  : IDENTIFIER { MkFeatureName $1 }
 -- | prefix FIXME
 -- | infix FIXME

rename_clause :: { RenameClause }
  : '{' renaming '}' { $2 }

renaming :: { RenameClause }
  : '^' class_name '.' feature_name
  { MkRenameClause $2 $4 }

feature_argument :: { [FeatureArgument] }
  : either_arrow identifier_list ':' type { [ MkFeatureArgument i $4 | i <- $2 ] }
  | either_arrow type { [MkFeatureArgument "" $2] }

either_arrow :: { Bool }
  : '->' { True }
  | '<-' { False }

identifier_list :: { [String] }
  : sep1(IDENTIFIER, ',') { $1 }

{-
--TODO - are these necessary if we do not allow free operators?
prefix  :  'prefix' '"' prefix_operator '"'
;

infix  :  'infix' '"' infix_operator '"'
;

--TODO - Add free_operator back?
prefix_operator  :  unary
;
--prefix_operator  :  UNARY | free_operator

infix_operator  :
  binary
--  infix_operator  :  binary | free_operator
;

------------------------------------------------
----------}
formal_generics :: { [FormalGeneric] }
  : '[' formal_generic_list ']' { $2 }

formal_generic_list :: { [FormalGeneric] }
  : sep1(formal_generic, ',') { $1 }

formal_generic :: { FormalGeneric }
  : formal_generic_name                 { MkFormalGeneric $1 Nothing }
  | formal_generic_name '->' class_type { MkFormalGeneric $1 (Just $3) }

formal_generic_name :: { String }
  : IDENTIFIER { $1 }

class_type :: { Type }
  : class_name opt(actual_generics)
  { MkType $1 (fromMaybe [] $2) }

actual_generics :: { [Type] }
  : '[' type_list ']' { $2 }

type_list :: { [Type] }
  : sep1(type, ',') { $1 }

--TODO - Conflict - class_type is essentially IDENTIFIER (actual_generics)?
--And formal_generic_name is IDENTIFIER
--type  :  class_type | formal_generic_name
type :: { Type }
  : IDENTIFIER opt(actual_generics)
  { MkType $1 (fromMaybe [] $2) }

{-
/**********************************************
 ***   Formal Assertions                    ***
 **********************************************/
--TODO correct this all for use with the new expression grammar
-}

assertion :: { [Expression] }
  : sep1f(assertion_clause, ';') { $1 }

assertion_clause :: { Expression }
  : boolean_expression { $1 }
-- | COMMENT
--TODO - Disallowing until revisiting this part of the grammar, as allowing comments here seems to make no sense

--TODO - replace expression here?
boolean_expression :: { Expression }
  : expression { $1 }

quantification :: { Quantification }
  : quantifier
    range_expression
    opt(restriction)
    proposition
  { MkQuantification $1 $2 $3 $4 }

quantifier :: { Quantifier }
  : 'for_all' { FORALL }
  | 'exists'  { EXISTS }

range_expression :: { [VariableRange] }
  : sep1f(variable_range, ';') { $1 }

restriction :: { Expression }
  : 'such_that' boolean_expression { $2 }

proposition :: { Expression }
  : 'it_holds' boolean_expression { $2 }

variable_range :: { VariableRange }
  : member_range { MemberRange $1 }
  | type_range   { TypeRange $1 }

member_range :: { MemberRange }
  : identifier_list 'member_of' expression
  { MkMemberRange $1 $3 }

type_range :: { TypeRange }
  : identifier_list ':' type
  { MkTypeRange $1 $3 }

------------------------------------------------

unqualified_call :: { UnqualifiedCall }
  : IDENTIFIER actual_arguments { MkUnqualifiedCall $1 $2 }
  | IDENTIFIER { MkUnqualifiedCall $1 [] }

actual_arguments :: { [Expression] }
  : '(' expression_list ')' { $2 }
  | '(' ')' { [] }

expression_list :: { [Expression] }
  : sep1(expression, ',') { $1 }

------------------------------------------------

--enumerated sets are allowed as an expression
--set_expression  :  enumerated_set
--                 ->
--                 | expression
--                ;

enumerated_set :: { [EnumerationElement] }
  : '{' enumeration_list '}' { $2 }

enumeration_list :: { [EnumerationElement] }
  : sep1(enumeration_element, ',') { $1 }

enumeration_element :: { EnumerationElement }
  : expression { Expression $1 }
  | interval   { Interval $1 }

interval :: { Interval }
  : integer_interval   { IntegerInterval $1 }
  | character_interval { CharacterInterval $1 }

integer_interval :: { IntegerInterval }
  : integer_constant '..' integer_constant
  { MkIntegerInterval $1 $3 }

character_interval :: { CharacterInterval }
  : character_constant '..' character_constant
  { MkCharacterInterval $1 $3 }

------------------------------------------------

constant :: { Constant }
  : manifest_constant { ManifestConstant $1 }
  | 'Current' { CurrentConstant }
  | 'Void'    { VoidConstant }
  | 'Result'  { ResultConstant }

manifest_constant :: { ManifestConstant }
  : boolean_constant   { BooleanConstant $1 }
  | character_constant { CharacterConstant $1 }
  | integer_constant   { IntegerConstant $1 }
  | real_constant      { RealConstant $1 }
  | MANIFEST_STRING    { StringConstant $1 }
  | enumerated_set     { SetConstant $1 }

boolean_constant :: { Bool }
  : 'true'  { True }
  | 'false' { False }

--Changed to lexer rule, as we greedily take any character preceded and followed by a '
character_constant :: { Char }
  : CHARACTER_CONSTANT { error "FIXME" }
{-
CHARACTER_CONSTANT :  '\'' v=. '\'';
-}

integer_constant :: { Integer }
  : '-' INTEGER { - $2 }
  | '+' INTEGER { $2 }
  | INTEGER { $1 }

real_constant :: { Double }
  : '-' REAL { - $2 }
  | '+' REAL { $2 }
  | REAL { $1 }

{-
/**********************************************
 ***   Dynamic Diagrams                     ***
 **********************************************/
-}

dynamic_diagram :: { DynamicDiagram }
  : 'dynamic_diagram' opt(extended_id) comment 'component' list(dynamic_component) 'end'
  { MkDynamicDiagram $5 $2 $3 }

dynamic_component :: { DynamicComponent }
  : scenario_description { ScenarioDescription $1 }
  | object_group         { ObjectGroup $1 }
  | object_stack         { ObjectStack $1 }
  | object               { ObjectInstance $1 }
  --FIXME | message_relation     { $1 }

------------------------------------------------

scenario_description :: { ScenarioDescription }
  : 'scenario' scenario_name comment 'action' list1(labelled_action) 'end'
  { MkScenarioDescription $2 $5 $3 }

labelled_action :: { LabelledAction }
  : action_label action_description
  { MkLabelledAction $1 $2 }

action_label :: { String }
  : MANIFEST_STRING { $1 }

action_description :: { String }
  : manifest_textblock { $1 }

scenario_name :: { String }
  : manifest_textblock { $1 }

------------------------------------------------

object_group :: { ObjectGroup }
  : nameless
    'object_group'
    group_name
    comment
    opt(group_components)
  { MkObjectGroup $1 $3 (fromMaybe [] $5) $4 }

nameless :: { Bool }
  : 'nameless'  { True }
  | {- empty -} { False }

group_components :: { [DynamicComponent] }
  : 'component' list1(dynamic_component) 'end' { $2 }

object_stack :: { ObjectStack }
  : 'object_stack' object_name comment
  { MkObjectStack $2 $3 }

object :: { ObjectInstance }
  : 'object' object_name comment
  { MkObjectInstance $2 $3 }
{-----------
------------------------------------------------

message_relation  :  caller 'calls' receiver (message_label)?
;

caller  :  dynamic_ref
;

receiver  :  dynamic_ref
;

--TODO - the below change fixes a conflict, and allows the same grammar
--...but we lose some information here as to what the dynamic ref is.
--Can this be fixed at a later point when going over the AST?
--dynamic_ref  :  (group_prefix)* dynamic_component_name
dynamic_ref  :  extended_id ('.' extended_id)*
;

--group_prefix  :  group_name '.'
--              ;

--TODO - similarly this rule matches the same grammar, but will we need to know
-- which we're actually matching?
--dynamic_component_name  :   object_name | group_name
dynamic_component_name  :
   (IDENTIFIER ('.' extended_id)?)
 | INTEGER
;
-}

object_name :: { ObjectName }
  : class_name                 { MkObjectName $1 Nothing }
  | class_name '.' extended_id { MkObjectName $1 (Just $3) }

group_name :: { String }
  : extended_id { $1 }
{-
message_label :: { String }
  : MANIFEST_STRING { $1 }

{-
/**********************************************
 ***   Notational Tuning                    ***
 **********************************************/
-}
--TODO - do we want any of this section currently?
notational_tuning :
   change_string_marks
 | change_concatenator
 | change_prefix
;

change_string_marks  :
  'string_marks' MANIFEST_STRING MANIFEST_STRING
;

change_concatenator  :
  'concatenator' MANIFEST_STRING
;

change_prefix  :
  'keyword_prefix' MANIFEST_STRING
;

{-
/**********************************************
 ***   Expressions                          ***
 **********************************************/
-}


---}

expression :: { Expression }
  : expr1 { $1 }
  | quantification { Quantification $1 }

expr1 :: { Expression }
  : unary_expression              { $1 }
  | constant                      { Constant $1 }
  | expr1 '<->'  expr1            { BinaryExp Equiv $1 $3 }
  | expr1 '->'   expr1            { BinaryExp Implies $1 $3 }
  | expr1 'and'  expr1            { BinaryExp And $1 $3 }
  | expr1 'or'   expr1            { BinaryExp Or $1 $3 }
  | expr1 'xor'  expr1            { BinaryExp Xor $1 $3 }
  | expr1 '<'    expr1            { BinaryExp Lt $1 $3 }
  | expr1 '>'    expr1            { BinaryExp Gt $1 $3 }
  | expr1 '<='   expr1            { BinaryExp Le $1 $3 }
  | expr1 '>='   expr1            { BinaryExp Ge $1 $3 }
  | expr1 '='    expr1            { BinaryExp Eq $1 $3 }
  | expr1 '/='   expr1            { BinaryExp Neq $1 $3 }
  | expr1 '+'    expr1            { BinaryExp Add $1 $3 }
  | expr1 '-'    expr1            { BinaryExp Sub $1 $3 }
  | expr1 '*'    expr1            { BinaryExp Mul $1 $3 }
  | expr1 '/'    expr1            { BinaryExp Div $1 $3 }
  | expr1 '//'   expr1            { BinaryExp IntDiv $1 $3 }
  | expr1 '\\\\' expr1            { BinaryExp Mod $1 $3 }
  | expr1 '^'    expr1            { BinaryExp Pow $1 $3 }
  | expr1 ':'    expr1            { BinaryExp HasType $1 $3 } -- FIXME "type"
  | expr1 'member_of' expr1       { BinaryExp MemberOf $1 $3 }
  | expr1 'not' 'member_of' expr1 { BinaryExp NotMemberOf $1 $4 }

unary_expression :: { Expression }
  : lowest_expression      { $1 }
  | unary unary_expression { UnaryExp $1 $2 }

lowest_expression :: { Expression }
  : unqualified_call                       { UnqualifiedCall $1 }
  | lowest_expression '.' unqualified_call { CallExp $1 $3 }
  | '(' expression ')'                     { $2 }

{---
{-
/**********************************************
 ***   Operators                            ***
 **********************************************/
-}

add_sub_op :: BinaryOp
  : '+' { ADD }
  | '-' { SUB }
-}
add_sub_op_unary :: { UnaryOp }
  : '+' { UAdd }
  | '-' { USub }
{-
and_or_xor_op :: BinaryOp
  : 'and' { AND }
  | 'or'  { OR }
  | 'xor' { XOR }
-}
unary :: { UnaryOp }
  : other_unary       { $1 }
  | add_sub_op_unary  { $1 }

other_unary :: { UnaryOp }
  : 'delta' { DELTA }
  | 'old'   { OLD }
  | 'not'   { NOT }
{-
binary :: BinaryOp
  : add_sub_op    { $1 }
  | mul_div_op    { $1 }
  | comparison_op { $1 }
  | mod_pow_op    { $1 }
  | and_or_xor_op { $1 }
  | '->'          { Implies }
  | '<->'         { Equiv }

comparison_op :: BinaryOp
  : '<'  { Lt }
  | '>'  { Gt }
  | '<=' { Le }
  | '>=' { Ge }
  | '='  { Eq }
  | '/=' { Neq }
  | 'member_of' { MemberOf }
  | 'not' 'member_of' { NotMemberOf }
  | ':'  { HasType }

mul_div_op :: BinaryOp
  : '*'  { Mul }
  | '/'  { Div }
  | '//' { IntDiv }

mod_pow_op :: BinaryOp
  : '\\\\' { Mod }
  | '^'    { Pow }

{-
/*############################################*
 ###   Lexer...                             ###
 ##############################################
 *############################################*/
-}

--FREE_OPERATOR  :  ~('"'|' '|'\n'|'\r'|'\t') ;

{-
/**********************************************
 ***   Strings                              ***
 **********************************************/
-}

--fragment
--CONTINUED_STRING :  '\\' NEWLINE (options {greedy=false;} : ~('"'|'\\') )*
--                    ;


MANIFEST_STRING : '"'
                  (options {greedy=false;} : ~('\n'|'\r'|'"'|'\\') )*
                  '"'
                ;

--MANIFEST_TEXTBLOCK :   '"'
--                       (options {greedy=false;} : ~('\n'|'\r'|'"'|'\\') )*
--                       ('\\' NEWLINE (options {greedy=false;} : ~('"'|'\\') )* )*
--                       '"'
--                   ;

MANIFEST_TEXTBLOCK_START  : '"' (options {greedy=false;} : ~('\n'|'\r'|'"'|'\\') )+ '\\' (' '|'\t')* NEWLINE
           								;

MANIFEST_TEXTBLOCK_MIDDLE  : '\\' (options {greedy=false;} : ~('"'|'\\') )+ '\\' (' '|'\t')* NEWLINE
            							 ;

MANIFEST_TEXTBLOCK_END  : '\\' (options {greedy=false;} : ~('"'|'\\') )+ '"'
         								;


manifest_textblock
:
  { --TODO warn when not MANIFEST_STRING where we desire a single block.
  }
   MANIFEST_STRING
 | MANIFEST_TEXTBLOCK_START MANIFEST_TEXTBLOCK_MIDDLE* MANIFEST_TEXTBLOCK_END
;

COMMENT  :  LINE_COMMENT+ { $channel=HIDDEN; }
         ;

fragment
LINE_COMMENT  :  COMMENT_START (options {greedy=false;} : .)* NEWLINE
              ;

fragment
COMMENT_START  : '--'
               ;

fragment
NEWLINE  :  '\r'? '\n'
         ;

{-
/**********************************************
 ***   Numbers                              ***
 **********************************************/
-}

INTEGER  :  (DIGIT)+
         ;

REAL  :  DIGIT+ '.' DIGIT+
      ;

fragment
DIGIT  :  '0'..'9'
       ;

{-
/**********************************************
 ***   Identifiers                          ***
 **********************************************/
-}
{- From the book:
   the identifier construct is defined as a sequence of alphanumeric -
   characters including underscore. an identifier must begin with an
   alphanumeric character and must not end with an underscore (whose
   purpose really is to mimic word separation). letter case is not
   significant, but using consistent style rules is important. -}

IDENTIFIER  : ALPHA (ALPHANUMERIC_OR_UNDERSCORE* ALPHANUMERIC)?
            ;



fragment
ALPHANUMERIC_OR_UNDERSCORE  : ALPHANUMERIC | UNDERSCORE
                            ;

fragment
UNDERSCORE  :  '_'
            ;

fragment
ALPHANUMERIC  :  ALPHA | DIGIT
              ;

fragment
ALPHA  : LOWER | UPPER
       ;

fragment
LOWER  : 'a'..'z'
       ;

fragment
UPPER  : 'A'..'Z'
       ;

{-
/**********************************************
 ***   Whitespace                           ***
 **********************************************/
-}
WHITESPACE  :  (' '|'\n'|'\r'|'\t')+ {$channel=HIDDEN;}
            ;

---------------------- END -----------------------}
{

happyError :: [Token] -> a
happyError _ = error "happyError"

}
