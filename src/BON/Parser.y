{
{- Adapted from Fintan Fairmichael's BON.g. -}

module BON.Parser where

import BON.Parser.AST
import BON.Parser.Position

}

%token
  EOF    { }
  IDENTIFIER { }
  INTEGER { }
  CHARACTER_CONSTANT { }
  MANIFEST_STRING  { }
  REAL { }
  comment { }
  manifest_textblock { }

  'Current'          { }
  'Void'             { }
  'Result'           { }
  'action'           { }
  'and'              { }
  'class'            { }
  'class_chart'      { }
  'client'           { }
  'cluster'          { }
  'cluster_chart'    { }
  'command'          { }
  'component'        { }
  'constraint'       { }
  'creates'          { }
  'creation_chart'   { }
  'creator'          { }
  'deferred'         { }
  'delta'            { }
  'description'      { }
  'dictionary'       { }
  'dynamic_diagram'  { }
  'effective'        { }
  'end'              { }
  'ensure'           { }
  'event'            { }
  'event_chart'      { }
  'exists'           { }
  'explanation'      { }
  'false'            { }
  'feature'          { }
  'for_all'          { }
  'incoming'         { }
  'indexing'         { }
  'inherit'          { }
  'interfaced'       { }
  'invariant'        { }
  'involves'         { }
  'it_holds'         { }
  'member_of'        { }
  'nameless'         { }
  'not'              { }
  'object'           { }
  'object_group'     { }
  'object_stack'     { }
  'old'              { }
  'or'               { }
  'outgoing'         { }
  'part'             { }
  'persistent'       { }
  'query'            { }
  'redefined'        { }
  'require'          { }
  'reused'           { }
  'root'             { }
  'scenario'         { }
  'scenario_chart'   { }
  'static_diagram'   { }
  'such_that'        { }
  'system_chart'     { }
  'true'             { }
  'xor'              { }
  ';'                { }
  ':'                { }
  ','                { }
  '.'                { }
  '('                { }
  ')'                { }
  '['                { }
  ']'                { }
  '{'                { }
  '}'                { }
  ':{'               { }
  '<->'              { }
  '->'               { }
  '<-'               { }
  '<'                { }
  '>'                { }
  '<='               { }
  '>='               { }
  '='                { }
  '/='               { }
  '+'                { }
  '-'                { }
  '*'                { }
  '/'                { }
  '//'               { }
  '\\\\'             { }
  '^'                { }
  '..'               { }
  '...'              { }

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
      { MkBonSourceFile ($1.spec_els) null (getLoc $1) }
  | indexing bon_specification EOF
      { MkBonSourceFile ($2.spec_els) $1 (getLoc ($1, $2)) }
  | EOF
      -- { addParseProblem(MissingElementParseError(getLoc $1, "at least one specification entry", "in source file", True)) }
      { MkBonSourceFile (Constants.NO_SPEC_ELEMS) null (getLoc $1) }
  | indexing EOF
      -- { addParseProblem(MissingElementParseError(getLoc ($2), "at least one specification entry", "in source file", True)) }
      { MkBonSourceFile(Constants.NO_SPEC_ELEMS, $1, getLoc ($1.start,$1.stop)) }

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
  { MkClassDictionary $2 $6 $3 $4 $5 (getLoc ($1, $7)) }

dictionary_entry :: { DictionaryEntry }
  : 'class' class_name 'cluster' cluster_name_list description
  { DictionaryEntry ($2.text) (rlist $4) $5 (getLoc ($1, $5)) }

------------------------------------------------

system_chart :: { ClusterChart }
  : 'system_chart'
    system_name
    opt(indexing)
    opt(explanation)
    opt(part)
    opt(cluster_entries)
    'end'
  { MkClusterChart $2 True [] (fromMaybe [] $6) $3 $4 $5 (getLoc ($1, $7)) }

explanation :: { Located String }
  : 'explanation' manifest_textblock { $2 }
  | 'explanation'
  { addParseProblem(MissingElementParseError(getLoc ($e), "explanation text", "after 'explanation'", false)); }

indexing :: { Indexing }
  : 'indexing' index_list { MkIndexing $2 (getLoc ($1, $2)) }
  | 'indexing'
  --{ addParseProblem(MissingElementParseError(getLoc ($1), "indexing entries", "after 'indexing'", False)) }
  { MkIndexing [] (getLoc ($1)) }

part :: { Located String }
  : 'part' MANIFEST_STRING { $2 }
  | 'part'
    --{ addParseProblem(MissingElementParseError(getLoc ($p), "part text", "after 'part'", false)); }
    { "" }

description :: { Located String }
  : 'description' manifest_textblock { $2 }

cluster_entries :: { [ClusterEntry] }
  : list1 (cluster_entry) { $1 }

cluster_entry :: { ClusterEntry }
  : 'cluster' cluster_name description
  { MkClusterEntry ($2.text) ($3.description) (getLoc ($1, $3.stop)) }

system_name :: { Located String }
  : IDENTIFIER { $1.text };

------------------------------------------------

index_list :: { [IndexClause] }
  : sep1f(index_clause, ';') { $1 }

index_clause :: { IndexClause }
  : IDENTIFIER ':' index_term_list
  { MkIndexClause $1 $3 (getLoc ($1, $3)) }
  | IDENTIFIER ':'
  { addParseProblem(MissingElementParseError(getLoc ($1), "index term(s)", "in index clause", True)) }

index_term_list :: { [Located String] }
  : sep1(index_string, ',') { $1 }

index_string :: { Located String }
  : manifest_textblock { $1 }


------------------------------------------------

cluster_chart :: { ClusterChart }
  : 'cluster_chart'
    cluster_name
    opt(indexing)
    opt(explanation)
    opt(part)
    opt(class_entries)
    opt(cluster_entries)
    'end'
  { MkClusterChart $2 (fromMaybe [] $6) (fromMaybe [] $7) $3 $4 $5 (getLoc ($1, $8)) }

class_entries :: { [ClassEntry] }
  : list1(class_entry) { $1 }

class_entry :: { ClassEntry }
  : 'class' class_name description { MkClassEntry $2 $3 (getLoc ($1, $3)) }

cluster_name :: { Located String }
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
  { MkClassChart $2 (fromMaybe [] $6) (fromMaybe [] $7) (fromMaybe [] $8) (fromMaybe [] $9) $3 $4 $5
    (getLoc ($1,$9)) }

inherits :: { [ClassName] }
  : 'inherit' class_name_list { $2 }
  | 'inherit'
  { addParseProblem(MissingElementParseError(getLoc ($1), "class name(s)", "in inherits clause", True)) }

queries :: { [LString] }
  : 'query' query_list { $2 }

commands :: { [LString] }
  : 'command' command_list { $2 }

constraints :: { [LString] }
  : 'constraint' constraint_list { rlist $2 }

query_list :: { [LString] }
  : sep1f(manifest_textblock, ',') { $1 }

command_list :: { [LString] }
  : sep1f(manifest_textblock, ',') { $1 }

constraint_list :: { [String] }
  : sep1f(manifest_textblock, ',') { $1 }

class_name_list :: { [ClassName] }
  : sep1(class_name, ',') { $1 }

cluster_name_list :: { [String] }
  : sep1(cluster_name, ',') { $1 }

class_or_cluster_name_list :: { [String] }
  : sep1(class_or_bracketed_cluster_name, ',') { $1 }

class_or_bracketed_cluster_name :: { String }
  : class_name { thing $1 }
  | '(' cluster_name ')' { $2 }

class_name :: { ClassName }
  : IDENTIFIER { MkClassName (thing $1) (getRange $1) }

------------------------------------------------

event_chart :: { EventChart }
  : 'event_chart'
     system_name
     opt(direction)
     opt(indexing)
     opt(explanation)
     opt(part)
     opt(event_entries)
     'end'
  { MkEventChart $2 $3 (fromMaybe [] $7) $4 $5 $6 (getLoc ($1, $>)) }

direction :: { Direction }
  : 'incoming' { Incoming }
  | 'outgoing' { Outgoing }

event_entries :: { [EventEntry] }
  : list1(event_entry) { $1 }

event_entry :: { EventEntry }
  : 'event'
    manifest_textblock
    'involves'
    class_or_cluster_name_list
  { MkEventEntry $2 $4 (getLoc ($1, $>)) }

------------------------------------------------

scenario_chart :: { ScenarioChart }
  : 'scenario_chart'
    system_name
    opt(indexing)
    opt(explanation)
    opt(part)
    opt(scenario_entries)
    'end'
  { MkScenarioChart $2 (fromMaybe [] $6) $3 $4 $5 (getLoc ($1, $>)) }

scenario_entries :: { [ScenarioEntry] }
  : list1(scenario_entry) { $1 }

scenario_entry :: { ScenarioEntry }
  : 'scenario' MANIFEST_STRING description
  { MkScenarioEntry $2 $3 (getLoc ($1,$3)) }

------------------------------------------------

creation_chart :: { CreationChart }
  : 'creation_chart'
    system_name
    opt(indexing)
    opt(explanation)
    opt(part)
    opt(creation_entries)
    'end'
  { MkCreationChart $2 (fromMaybe [] $6) $3 $4 $5 (getLoc ($1, $>)) }

creation_entries :: { [CreationEntry] }
  : list1(creation_entry) { $1 }

creation_entry :: { CreationEntry }
  : 'creator' class_name 'creates' class_or_cluster_name_list
  { MkCreationEntry $2 $4 (getLoc ($1, $>)) }

{-
/**********************************************
 ***   Static Diagrams                      ***
 **********************************************/
-}

static_diagram :: { StaticDiagram }
  : 'static_diagram' opt(extended_id) comment 'component' static_block 'end'
  { MkStaticDiagram $5 $2 $3 (getLoc ($1, $>)) }

extended_id :: { Located String }
  : IDENTIFIER { $1 }
  | INTEGER { fmap show $1 }

static_block :: { [StaticComponent] }
  : list(static_component) { $1 }

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
  { MkCluster $2 (fromMaybe [] $5) $3 $4 (getLoc ($1, $2)) }

reused :: { Bool }
  : 'reused'    { True }
  | {- empty -} { False }

cluster_components :: { [StaticComponent] }
  : 'component' static_block 'end' { $2 }

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
  { MkClass $3 (fromMaybe [] $4) $1 $9 $5 $6 $7 $8 (getLoc ($1, $>)) }

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

{--------------
comment :: [String comment] :
  { $comment = lookForCommentBefore(); }
-------------}

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
  { MkInheritanceRelation $1 $4 $3 $5 (getLoc ($1, $>)) }

multiplicity_braces :: { Integer }
  : '{' multiplicity '}' { $2 }

client_relation :: { ClientRelation }
  : client
    'client'
    opt(client_entities)
    opt(type_mark)
    supplier
    opt(semantic_label)
  { MkClientRelation $1 $5 $3 $4 $6 (getLoc ($1, $>)) }

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
    { MkSupplierIndirection (Just $1) $3 (getLoc ($1, $3)) }
  | generic_indirection
    { MkSupplierIndirection Nothing $1 (getLoc $1) }

indirection_feature_part :: { IndirectionFeaturePart }
  : feature_name { FeatureName $1 }
  | indirection_feature_list { IndirectionFeatureList $1 }

indirection_feature_list :: { IndirectionFeatureList }
  : '(' feature_name_list ')'
  { MkIndirectionFeatureList $2 (getLoc ($1, $3)) }

parent_indirection :: { ParentIndirection }
  : '->' generic_indirection
  { MkParentIndirection $2 (getLoc $2) }

------------------------------------------------

generic_indirection :: { GenericIndirection }
--  formal_generic_name
                       --NB - changed the below... both are IDENTIFIERs
-- |
  : indirection_element
  { MkGenericIndirection $1 (getLoc $1) }

named_indirection :: { NamedIndirection }
  : class_name '[' indirection_list ']'
   { MkNamedIndirection $1 $3 (getLoc ($1, $>)) }

indirection_list :: { [IndirectionElement] }
  : sep1(indirection_element, ',') { $1 }

indirection_element :: { IndirectionElement }
  : '...' { CompactedIndirectionElementImpl (getLoc $1) }
  | named_indirection { NamedIndirection $1 }
  | class_name { ClassName $1 }

type_mark :: { TypeMark }
  : ':' { TypeMarkHASTYPE (getLoc $1) }
  | ':{' { TypeMarkAGGREGATE (getLoc $1) }
  | shared_mark { $1 }

shared_mark :: { TypeMark }
  : ':' '(' multiplicity ')'
  { TypeMarkSHAREDMARK $3 (getLoc ($1, $>)) }

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
  : static_component_name { MkStaticRef [] $1 (getLoc $1) }
  | cluster_prefix static_component_name { MkStaticRef $1 $2 (getLoc ($1, $2)) }

cluster_prefix :: { [StaticRefPart] }
  : cluster_name { [$1] }
  | cluster_prefix '.' cluster_name { $1 ++ [$3] }

--TODO - class_name and cluster_name are both just IDENTIFIERs.
--static_component_name  :  class_name | cluster_name
static_component_name :: { StaticRefPart }
  : IDENTIFIER { MkStaticRefPart $1 }

multiplicity :: { Located Integer }
  : INTEGER { $1 }

semantic_label :: { Located String }
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
    opt(features)
    opt(class_invariant)
    'end'
  { MkClassInterface (fromMaybe [] $3) (fromMaybe [] $2) (fromMaybe [] $4) (Just $1) (getLoc ($1, $>)) }

class_interface_start_inherit :: { ClassInterface }
  : parent_class_list
    opt(features)
    opt(class_invariant)
    'end'
  { MkClassInterface (fromMaybe [] $2) $1 (fromMaybe [] $3) Nothing (getLoc ($1, $>)) }

class_interface_start_features :: { ClassInterface }
  : features
    opt(class_invariant)
    'end'
  { MkClassInterface $1 [] (fromMaybe [] $2) Nothing (getLoc ($1, $>)) }

class_interface_start_invariant :: { ClassInterface }
  : class_invariant
    'end'
  { MkClassInterface [] [] $1 Nothing (getLoc ($1, $>)) }

class_invariant :: { [Expression] }
  : 'invariant' assertion { $2 }

parent_class_list :: { [Type] }
  : 'inherit' sep1f(class_type, ';') { $2 }
{-
|
  i='inherit'
  { addParseProblem(new MissingElementParseError(getLoc ($i), "class name(s)", "in inherits clause", true)); }
-}

features :: { [Feature] }
  : list1(feature_clause) { $1 }

------------------------------------------------

feature_clause :: { Feature }
  : 'feature'
    opt(selective_export)
    comment
    feature_specifications
  { MkFeature $4 (fromMaybe [] $2) $3 (getLoc ($1, $>)) }

feature_specifications :: { [FeatureSpecification] }
  : list1(feature_specification) { $1 }

feature_specification :: { FeatureSpecification }
  : feature_specification_modifier
    feature_name_list
    opt(has_type)
    opt(rename_clause)
    comment
    opt(feature_arguments)
    opt(contract_clause)
  { MkFeatureSpecification $1 $2 (fromMaybe [] $6) $7 $3 $4 $5 (getLoc ($1, $>)) }

feature_specification_modifier :: { FeatureSpecificationModifier }
  : 'deferred'  { FeatureSpecDEFERRED }
  | 'effective' { FeatureSpecEFFECTIVE }
  | 'redefined' { FeatureSpecREDEFINED }
  | {- empty -} { FeatureSpecNONE }

has_type :: { HasType }
  : type_mark type { MkHasType $1 $2 (getLoc ($1, $2)) }
  | type_mark 'Void' { MkHasType $1 (MkType "Void" (getLoc $2)) (getLoc ($1, $2)) }

------------------------------------------------

contract_clause :: { ContractClause }
  : contracting_conditions 'end' { $1 }

--NB. Rewritten from precondition | postcondition | pre_and_post
contracting_conditions :: { ContractClause }
  : precondition { MkContractClause $1 [] (getLoc $1) }
  | postcondition { MkContractClause [] $1 (getLoc $1) }
  | precondition postcondition  { MkContractClause $1 $2 (getLoc ($1, $2)) }

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
  : IDENTIFIER { MkFeatureName $1 (getLoc $1) }
 -- | prefix FIXME
 -- | infix FIXME

rename_clause :: { RenameClause }
  : '{' renaming '}' { $2 }

renaming :: { RenameClause }
  : '^' class_name '.' feature_name
  { MkRenameClause $2 $4 (getLoc ($1, $>)) }

feature_arguments :: { [FeatureArgument] }
  : list1(feature_argument) { $1 }

feature_argument :: { [FeatureArgument] }
  : either_arrow identifier_list ':' type { [ MkFeatureArgument i $4 (getLoc ($1, $>)) | i <- $2 ] }
  | either_arrow type { [MkFeatureArgument "" $2 (getLoc $2)] }

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
  : formal_generic_name                 { MkFormalGeneric $1 Nothing (getLoc $1) }
  | formal_generic_name '->' class_type { MkFormalGeneric $1 (Just $3) (getLoc ($1, $3)) }

formal_generic_name :: { String }
  : IDENTIFIER { $1 }

class_type :: { Type }
  : class_name opt(actual_generics)
  { MkType $1 (fromMaybe [] $2) (getLoc ($1, $2)) }

actual_generics :: { [Type] }
  : '[' type_list ']' { $2 }

type_list :: { [Type] }
  : sep1(type, ',') { $1 }

--TODO - Conflict - class_type is essentially IDENTIFIER (actual_generics)?
--And formal_generic_name is IDENTIFIER
--type  :  class_type | formal_generic_name
type :: { Type }
  : IDENTIFIER opt(actual_generics)
  { MkType $1 (fromMaybe [] $2) (getLoc ($1, $>)) }

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
  { MkQuantification $1 $2 $3 $4 (getLoc ($1, $>)) }

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
  { MkMemberRange $1 $3 (getLoc ($1.start,$3.stop)) }

type_range :: { TypeRange }
  : identifier_list ':' type
  { MkTypeRange $1 $3 (getLoc ($1.start,$3.stop)) }

------------------------------------------------

unqualified_call :: { UnqualifiedCall }
  : IDENTIFIER actual_arguments { MkUnqualifiedCall $1 $2 (getLoc ($1, $2)) }
  | IDENTIFIER { MkUnqualifiedCall $1 [] (getLoc $1) }

actual_arguments :: { [Expression] }
  : '(' expression_list ')' { rlist $2 }
  | '(' ')' { [] }

expression_list :: { RList Expression }
  : expression_list ',' expression { RCons $1 $3 }
  | expression { rsingle $1 }

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
  { MkIntegerInterval $1 $3 (getLoc ($1, $3)) }

character_interval :: { CharacterInterval }
  : character_constant '..' character_constant
  { MkCharacterInterval $1 $3 (getLoc ($1, $3)) }

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
  : CHARACTER_CONSTANT { $1 }
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
  : 'dynamic_diagram' opt(extended_id) comment 'component' opt(dynamic_block) 'end'
  { MkDynamicDiagram (fromMaybe [] $5) $2 $3 (getLoc ($1, $6)) }

dynamic_block :: { [DynamicComponent] }
  : list1(dynamic_component) { $1 }

dynamic_component :: { DynamicComponent }
  : scenario_description { ScenarioDescription $1 }
  | object_group         { ObjectGroup $1 }
  | object_stack         { ObjectStack $1 }
  | object               { ObjectInstance $1 }
  --FIXME | message_relation     { $1 }

------------------------------------------------

scenario_description :: { ScenarioDescription }
  : 'scenario' scenario_name comment 'action' labelled_actions 'end'
  { MkScenarioDescription $2 $5 $3 (getLoc ($1, $6)) }

labelled_actions :: { [LabelledAction] }
  : list1(labelled_action) { $1 }

labelled_action :: { LabelledAction }
  : action_label action_description
  { MkLabelledAction $1 $2 (getLoc ($1, $2)) }

action_label :: { String }
  : MANIFEST_STRING { $1 }

action_description :: { String }
  : manifest_textblock { $1 }

scenario_name :: { String }
  : manifest_textblock { $1 }
{-
------------------------------------------------

--------}
object_group :: { ObjectGroup }
  : nameless
    'object_group'
    group_name
    comment
    opt(group_components)
  { MkObjectGroup $1 $3 (fromMaybe [] $5) $4 (getLoc ($1, $>)) }

nameless :: { Bool }
  : 'nameless'  { True }
  | {- empty -} { False }

group_components :: { [DynamicComponent] }
  : 'component' dynamic_block 'end' { $2 }

object_stack :: { ObjectStack }
  : 'object_stack' object_name comment
  { MkObjectStack $2 $3 (getLoc ($1, $2)) }

object :: { ObjectInstance }
  : 'object' object_name comment
  { MkObjectInstance $2 $3 (getLoc ($1, $2)) }
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
  : class_name                 { MkObjectName $1 Nothing (getLoc $1) }
  | class_name '.' extended_id { MkObjectName $1 (Just $3) (getLoc ($1, $3)) }

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
  | unary unary_expression { UnaryExp $1 $2 (getLoc ($1, $2)) }

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
