#include "definitions.h"

#include "DataNode.h"
#include "fmt/core.h"

#include <cassert>
#include <unordered_map>

namespace {

const lsp::Type Double{lsp::Type::Double};
const lsp::Type Int{lsp::Type::Int};
const lsp::Type String{lsp::Type::String};
// Types with annotations.
const lsp::Type Conversation{lsp::Type::String, "conversation"};
const lsp::Type Effect{lsp::Type::String, "effect"};
const lsp::Type Event{lsp::Type::String, "event"};
const lsp::Type Fleet{lsp::Type::String, "fleet"};
const lsp::Type Galaxy{lsp::Type::String, "galaxy"};
const lsp::Type Government{lsp::Type::String, "government"};
const lsp::Type Hazard{lsp::Type::String, "hazard"};
const lsp::Type News{lsp::Type::String, "news"};
const lsp::Type Outfit{lsp::Type::String, "outfit"};
const lsp::Type Outfitter{lsp::Type::String, "outfitter"};
const lsp::Type Phrase{lsp::Type::String, "phrase"};
const lsp::Type Planet{lsp::Type::String, "planet"};
const lsp::Type Ship{lsp::Type::String, "ship"};
const lsp::Type Shipyard{lsp::Type::String, "shipyard"};
const lsp::Type Sound{lsp::Type::String, "sound"};
const lsp::Type Sprite{lsp::Type::String, "sprite"};
const lsp::Type System{lsp::Type::String, "system"};
// Types with keywords.
const lsp::Type PenaltyForType{.Kind = lsp::Type::String,
                               .Keywords = {"assist", "disable", "board",
                                            "capture", "destroy", "atrocity"}};
const lsp::Type Alignment{
    .Kind = lsp::Type::String,
    .Keywords = {"left", "top", "right", "bottom", "center"}};
const lsp::Type MissionToType{.Kind = lsp::Type::String,
                              .Keywords = {"offer", "complete", "fail"}};
const lsp::Type MissionOnType{.Kind = lsp::Type::String,
                              .Keywords = {"complete", "enter", "accept",
                                           "decline", "fail", "abort", "defer",
                                           "visit", "stopover", "waypoint"}};

// Common node definitions that appear everywhere.
lsp::NodeDefinition FullSprite(std::string_view Name) {
  return {.Name = Name,
          .ParameterTypes = {Sprite},
          .Children = {
              {.Name = "frame rate", .ParameterTypes = {Double}},
              {.Name = "frame time", .ParameterTypes = {Double}},
              {.Name = "delay", .ParameterTypes = {Double}},
              {.Name = "scale", .ParameterTypes = {Double}},
              {.Name = "start frame", .ParameterTypes = {Double}},
              {.Name = "random start frame"},
              {.Name = "no repeat"},
              {.Name = "rewind"},
          }};
}
lsp::NodeDefinition WeaponNode{.Name = "weapon"}; // FIXME
lsp::NodeDefinition HazardWeapon{
    .Name = "weapon",
    .Children = {{.Name = "shield damage", .ParameterTypes = {Double}},
                 {.Name = "hull damage", .ParameterTypes = {Double}},
                 {.Name = "fuel damage", .ParameterTypes = {Double}},
                 {.Name = "heat damage", .ParameterTypes = {Double}},
                 {.Name = "energy damage", .ParameterTypes = {Double}},
                 {.Name = "ion damage", .ParameterTypes = {Double}},
                 {.Name = "disruption damage", .ParameterTypes = {Double}},
                 {.Name = "slowing damage", .ParameterTypes = {Double}},
                 {.Name = "relative shield damage", .ParameterTypes = {Double}},
                 {.Name = "relative hull damage", .ParameterTypes = {Double}},
                 {.Name = "relative fuel damage", .ParameterTypes = {Double}},
                 {.Name = "relative heat damage", .ParameterTypes = {Double}},
                 {.Name = "relative energy damage", .ParameterTypes = {Double}},
                 {.Name = "piercing", .ParameterTypes = {Double}},
                 {.Name = "hit force", .ParameterTypes = {Double}},
                 {.Name = "gravitational"},
                 {.Name = "blast radius", .ParameterTypes = {Double}},
                 {.Name = "damage dropoff",
                  .ParameterTypes = {Double, Double},
                  .OptionalIndex = 1},
                 {.Name = "dropoff modifier", .ParameterTypes = {Double}},
                 {.Name = "target effect",
                  .ParameterTypes = {Effect, Double},
                  .OptionalIndex = 1}}};
} // namespace

// The node definitions that define the syntax for each possible definition.
const std::unordered_map<std::string_view, lsp::NodeDefinition>
    lsp::Definitions{
        {"color",
         {.Name = "color", .ParameterTypes = {Double, Double, Double, Double}}},
        {"conversation", // TODO: Finish syntax tree.
         {.Name = "conversation",
          .ParameterTypes = {String},
          .Children = {{.Name = "scene", .ParameterTypes = {Sprite}},
                       {.Name = "label", .ParameterTypes = {String}},
                       {.Name = "choice", .Children = {{.BaseType = String}}},
                       {.Name = "name"},
                       {.Name = "branch"}}}},
        {"effect",
         {.Name = "effect",
          .ParameterTypes = {String},
          .Children = {FullSprite("sprite"),
                       {.Name = "sound", .ParameterTypes = {Sound}},
                       {.Name = "lifetime", .ParameterTypes = {Double}},
                       {.Name = "random lifetime", .ParameterTypes = {Double}},
                       {.Name = "velocity scale", .ParameterTypes = {Double}},
                       {.Name = "random velocity", .ParameterTypes = {Double}},
                       {.Name = "random angle", .ParameterTypes = {Double}},
                       {.Name = "random spin", .ParameterTypes = {Double}},
                       {.Name = "random frame rate",
                        .ParameterTypes = {Double}}}}},
        {"event",
         {.Name = "event",
          .ParameterTypes = {String},
          .Children = {{.Name = "date",
                        .ParameterTypes = {Double, Double, Double}},
                       {.Name = "unvisit", .ParameterTypes = {System}},
                       {.Name = "visit", .ParameterTypes = {System}},
                       {.Name = "unvisit planet", .ParameterTypes = {Planet}},
                       {.Name = "visit planet", .ParameterTypes = {Planet}},
                       {.Name = "fleet", .ParameterTypes = {Fleet}},
                       {.Name = "galaxy", .ParameterTypes = {Galaxy}},
                       {.Name = "government", .ParameterTypes = {Government}},
                       {.Name = "outfitter", .ParameterTypes = {Outfitter}},
                       {.Name = "news", .ParameterTypes = {News}},
                       {.Name = "planet", .ParameterTypes = {Planet}},
                       {.Name = "shipyard", .ParameterTypes = {Shipyard}},
                       {.Name = "system", .ParameterTypes = {System}}}}},
        {"fleet",
         {.Name = "fleet",
          .ParameterTypes = {String},
          .Children =
              {
                  {.Name = "government", .ParameterTypes = {Government}},
                  {.Name = "names", .ParameterTypes = {Phrase}},
                  {.Name = "fighters", .ParameterTypes = {Phrase}},
                  {.Name = "cargo", .ParameterTypes = {Int}},
                  {.Name = "commodities",
                   .ParameterTypes = {String, String},
                   .VariableIndex = 1},
                  {.Name = "outfitters",
                   .ParameterTypes = {Outfitter, Outfitter},
                   .VariableIndex = 1},
                  // TODO: personality
                  // TODO: variant
              }}},
        {"galaxy",
         {.Name = "galaxy",
          .ParameterTypes = {String},
          .Children = {{.Name = "pos", .ParameterTypes = {Double, Double}},
                       {.Name = "sprite", .ParameterTypes = {Sprite}}}}},
        {"government",
         {.Name = "government",
          .ParameterTypes = {String},
          .Children =
              {{.Name = "display name", .ParameterTypes = {String}},
               {.Name = "swizzle", .ParameterTypes = {Int}},
               {.Name = "color", .ParameterTypes = {Double, Double, Double}},
               {.Name = "player reputation", .ParameterTypes = {Double}},
               {.Name = "crew attack", .ParameterTypes = {Double}},
               {.Name = "crew defense", .ParameterTypes = {Double}},
               {.Name = "attitude toward",
                .Children = {{.BaseType = Government,
                              .ParameterTypes = {Double}}},
                .VariableChildIndex = 0},
               {.Name = "penalty for",
                .Children = {{.BaseType = PenaltyForType,
                              .ParameterTypes = {Double}}},
                .VariableChildIndex = 0},
               {.Name = "bribe", .ParameterTypes = {Double}},
               {.Name = "fine", .ParameterTypes = {Double}},
               {.Name = "enforces"}, // FIXME
               {.Name = "death sentence", .ParameterTypes = {Conversation}},
               {.Name = "friendly hail", .ParameterTypes = {Phrase}},
               {.Name = "friendly disabled hail", .ParameterTypes = {Phrase}},
               {.Name = "hostile hail", .ParameterTypes = {Phrase}},
               {.Name = "hostile disabled hail", .ParameterTypes = {Phrase}},
               {.Name = "language", .ParameterTypes = {String}},
               {.Name = "raid", .ParameterTypes = {Fleet}}}}},
        {"hazard",
         {.Name = "hazard",
          .ParameterTypes = {String},
          .Children = {HazardWeapon,
                       {.Name = "constant strength"},
                       {.Name = "period", .ParameterTypes = {Int}},
                       {.Name = "duration",
                        .ParameterTypes = {Int, Int},
                        .OptionalIndex = 1},
                       {.Name = "strength",
                        .ParameterTypes = {Double, Double},
                        .OptionalIndex = 1},
                       {.Name = "range",
                        .ParameterTypes = {Double, Double},
                        .OptionalIndex = 1},
                       {.Name = "environmental effect",
                        .ParameterTypes = {Effect, Int},
                        .OptionalIndex = 1}}}},
        {"interface",
         {// TODO: Finish this.
          .Name = "interface",
          .ParameterTypes = {String},
          .Children = {{.Name = "anchor",
                        .ParameterTypes = {Alignment},
                        .VariableIndex = 0},
                       {.Name = "value", .ParameterTypes = {String, Double}},
                       {.Name = "point"},
                       {.Name = "box"},
                       {.Name = "visible"},
                       {.Name = "active"}}}},
        {"minable",
         {.Name = "minable",
          .ParameterTypes = {String},
          .Children =
              {
                  {.Name = "sprite", .ParameterTypes = {Sprite}},
                  {.Name = "hull", .ParameterTypes = {Double}},
                  {.Name = "payload",
                   .ParameterTypes = {Outfit, Double},
                   .OptionalIndex = 1},
                  {.Name = "explode",
                   .ParameterTypes = {Effect, Double},
                   .OptionalIndex = 1},
              }}},
        {"mission",
         {.Name = "mission",
          .ParameterTypes = {String},
          .Children =
              {
                  {.Name = "name", .ParameterTypes = {String}},
                  {.Name = "uuid",
                   .ParameterTypes = {String}}, // TODO: Consider a UUID type.
                  {.Name = "description", .ParameterTypes = {String}},
                  {.Name = "blocked", .ParameterTypes = {String}},
                  {.Name = "deadline",
                   .ParameterTypes = {Double, Double, Double},
                   .OptionalIndex = 0},
                  {.Name = "cargo",
                   .ParameterTypes = {String, Double, Double, Double},
                   .OptionalIndex =
                       2}, // TODO: add support for deprecated children.
                  {.Name = "passengers",
                   .ParameterTypes = {Double, Double, Double},
                   .OptionalIndex = 1},
                  {.Name = "illegal",
                   .ParameterTypes = {Double, Double},
                   .OptionalIndex = 1},
                  {.Name = "stealth"},
                  {.Name = "invisible"},
                  {.Name = "priority"},
                  {.Name = "minor"},
                  {.Name = "autosave"},
                  {.Name = "job"},
                  {.Name = "landing"},
                  {.Name = "boarding"},
                  {.Name = "repeat",
                   .ParameterTypes = {Int},
                   .OptionalIndex = 0},
                  {.Name = "clearance",
                   .ParameterTypes = {String},
                   .OptionalIndex = 0}, // TODO: finish
                  {.Name = "infiltrating"},
                  {.Name = "failed"},
                  {.Name = "to",
                   .ParameterTypes = {MissionToType}}, // TODO: finish
                  {.Name = "source",
                   .ParameterTypes = {Planet},
                   .OptionalIndex = 0},    // TODO: finish
                  {.Name = "destination"}, // TODO: finish
                  {.Name = "waypont"},     // TODO: finish
                  {.Name = "stopover"},    // TODO: finish
                  {.Name = "npc", .ParameterTypes = {String}},
                  {.Name = "on",
                   .ParameterTypes = {MissionOnType, System},
                   .OptionalIndex = 1}, // TODO: finish
              }}},
        {"outfit",
         {.Name = "outfit",
          .ParameterTypes = {String},
          .Children =
              {
                  {.Name = "category", .ParameterTypes = {String}},
                  {.Name = "plural", .ParameterTypes = {String}},
                  FullSprite("flare sprite"),
                  FullSprite("reverse flare sprite"),
                  FullSprite("steering flare sprite"),
                  {.Name = "flare sound", .ParameterTypes = {Sound}},
                  {.Name = "reverse flare sound", .ParameterTypes = {Sound}},
                  {.Name = "steering flare sound", .ParameterTypes = {Sound}},
                  {.Name = "afterburner effect", .ParameterTypes = {Effect}},
                  {.Name = "jump effect", .ParameterTypes = {Effect}},
                  {.Name = "hyperdrive sound", .ParameterTypes = {Sound}},
                  {.Name = "hyperdrive in sound", .ParameterTypes = {Sound}},
                  {.Name = "hyperdrive out sound", .ParameterTypes = {Sound}},
                  {.Name = "jump sound", .ParameterTypes = {Sound}},
                  {.Name = "jump in sound", .ParameterTypes = {Sound}},
                  {.Name = "jump out sound", .ParameterTypes = {Sound}},
                  {.Name = "floatsam sprite", .ParameterTypes = {Sprite}},
                  {.Name = "thumbnail", .ParameterTypes = {Sprite}},
                  WeaponNode,
                  {.Name = "ammo", .ParameterTypes = {Outfit}},
                  {.Name = "description", .ParameterTypes = {String}},
                  {.Name = "cost", .ParameterTypes = {Double}},
                  {.Name = "mass", .ParameterTypes = {Double}},
                  {.Name = "licenses",
                   .ParameterTypes = {String},
                   .VariableIndex = 0,
                   .Children = {{.BaseType = String}},
                   .VariableChildIndex = 0},
                  {.Name = "jump range", .ParameterTypes = {Double}},
              }}},
        {"outfitter",
         {.Name = "outfitter",
          .ParameterTypes = {String},
          .Children = {{.BaseType = Outfit}},
          .VariableChildIndex = 0}},
        {"person",
         {.Name = "person",
          .ParameterTypes = {String},
          .Children =
              {
                  {.Name = "system", .ParameterTypes = {System}},
                  {.Name = "frequency", .ParameterTypes = {Double}},
                  {.Name = "ship", .ParameterTypes = {Ship}}, // TODO: finish
                  {.Name = "government", .ParameterTypes = {Government}},
                  {.Name = "personality",
                   .ParameterTypes = {String},
                   .VariableIndex = 0}, // TODO: finish
              }}},
        {"phrase", {.Name = "phrase"}}, // TODO: finish
        {
            "planet",
            {.Name = "planet",
             .ParameterTypes = {String},
             .Children =
                 {{.Name = "attributes",
                   .ParameterTypes = {String},
                   .VariableIndex = 0},
                  {.Name = "shipyard", .ParameterTypes = {Shipyard}},
                  {.Name = "outfitter", .ParameterTypes = {Outfitter}},
                  {.Name = "landscape", .ParameterTypes = {Sprite}},
                  {.Name = "government", .ParameterTypes = {Government}},
                  {.Name = "required reputation", .ParameterTypes = {Double}},
                  {.Name = "bribe", .ParameterTypes = {Double}},
                  {.Name = "security", .ParameterTypes = {Double}},
                  {.Name = "tribute",
                   .ParameterTypes = {Double},
                   .Children =
                       {{.Name = "threshold",
                         .ParameterTypes = {Double}}}}}}, // TODO: finish
        }};

const std::unordered_map<
    std::string_view,
    std::unordered_map<std::string_view,
                       std::pair<std::string_view, std::string_view>>>
    lsp::NodeTooltips{
        {"color",
         {{"color",
           {"<name> <red> <green> <blue> <alpha>", "A color definition."}}}},
        {"conversation", // TODO: Finish
         {{"conversation", {"<name>", ""}},
          {"scene", {"<image>", ""}},
          {"label", {"<name>", ""}},
          {"choice", {"", ""}},
          {"name", {"", ""}},
          {"branch", {"<if true> [if false]", ""}},
          {"apply", {"", ""}}}},
        {"effect",
         {{"effect", {"<name", ""}},
          {"scale", {"<number#>", ""}},
          {"frame rate", {"<fps#>", "frames per second"}},
          {"start frame", {"<number#>", ""}},
          {"random start frame", {"", ""}},
          {"no repeat", {"", ""}},
          {"rewind", {"", ""}},
          {"sound", {"<name>", ""}},
          {"lifetime", {"<frames#>", ""}},
          {"random lifetime", {"<frames#>", ""}},
          {"velocity scale",
           {"<scale#>",
            "Without this, an effect will have the same velocity as the ship "
            "or projectile that \\\"created\\\" it. If this is defined, its "
            "velocity will be multiplied by this amount. Use a negative number "
            "to have the effect \\\"bounce\\\" in the opposite direction from the "
            "projectile."}},
          {"random velocity", {"<velocity#>", ""}},
          {"random angle", {"<degrees#>", ""}},
          {"random spin", {"<degrees#>", ""}},
          {"random frame rate", {"<fps#>", ""}}}}};

lsp::Type lsp::Type::FromString(std::string_view String) noexcept {
  // Try to parse as a number. If it's not a number then it's a string.
  // [+-]?[0-9]*[.]?[0-9]*([eE][+-]?[0-9]*)?
  if (String.empty())
    return {Type::String};
  std::size_t I = 0;

  bool Decimal = false;
  if (String[I] == '+' || String[I] == '-')
    ++I;
  while (I < String.size() && String[I] >= '0' && String[I] <= '9')
    ++I;
  if (String[I] == '.') {
    ++I;
    Decimal = true;
  }
  while (I < String.size() && String[I] >= '0' && String[I] <= '9')
    ++I;
  if (String[I] == 'e' || String[I] == 'E') {
    if (String[I] == '+' || String[I] == '-')
      ++I;
    while (I < String.size() && String[I] >= '0' && String[I] <= '9')
      ++I;
  }

  if (I == String.size() && !Decimal)
    return {Type::Int};
  if (I == String.size())
    return {Type::Double};
  return {Type::String};
}

bool lsp::Type::VerifyType(const Type &Other,
                           std::string_view Contents) const noexcept {
  if (Kind == String && Other.Kind == String) {
    // Both are strings, check if we have a keyword.
    if (!Keywords.empty() && Keywords.count(Contents))
      return true;
    return Keywords.empty();
  }
  // Same type is a-ok.
  else if (*this == Other)
    return true;
  // Int -> Double conversion is allowed.
  else if (Kind == Double && Other.Kind == Int)
    return true;
  return false;
}

lsp::Type::operator std::string_view() const noexcept {
  switch (Kind) {
  case Type::Double:
    return "double";
  case Type::Int:
    return "int";
  case Type::String:
    return "string";
  default:
#ifndef _WIN32
    __builtin_unreachable();
#else
    return "";
#endif
  }
}

void lsp::CheckLine(std::vector<lsp::Diagnostic> &Diagnostics,
                    const std::vector<lsp::DataNode *> &Nodes) {
  if (Nodes.empty())
    return;

  // 1) Go through the parent hierarchy to find the curent line and to see if it
  // matches.
  const lsp::NodeDefinition *Def = nullptr;
  auto It = Definitions.find(Nodes.front()->Parameters.front());
  if (It == Definitions.end()) {
    // One node in the hierarchy doesn't exist.
    if (Nodes.size() == 1) {
      // Only generate diagnostic once.
      auto &Diag = Diagnostics.emplace_back(*Nodes.front(), 0);
      Diag.Kind = lsp::Diagnostic::Error;
      Diag.Message =
          fmt::format("'{}' doesn't exist.", Nodes.front()->Parameters.front());
    }
    return;
  }
  Def = &It->second;

  // Now go through the children.
  for (auto It = Nodes.begin() + 1; It != Nodes.end(); ++It) {
    auto Search = std::find_if(Def->Children.begin(), Def->Children.end(),
                               [&It](const auto &Node) {
                                 return Node.Name == (*It)->Parameters.front();
                               });
    if (Search == Def->Children.end()) {
      // This node doesn't exist. If it is the current node, then we emit a
      // diagnostic.
      if (It + 1 == Nodes.end()) {
        auto &Diag = Diagnostics.emplace_back(**It, 0);
        Diag.Kind = lsp::Diagnostic::Error;
        Diag.Message = fmt::format("Invalid child '{}' for '{}'.",
                                   (*It)->Parameters.front(),
                                   (*It)->Parent->Parameters.front());
      }
      return;
    }
    Def = &*Search;
  }

  // 2) Now that we have the definition of the current node, we type check it.
  // Represents the index of the first optional parameters.
  // We need to handle them specially because they're optional.
  std::size_t Optional = Def->OptionalIndex == -1 ? Def->ParameterTypes.size()
                                                  : Def->OptionalIndex;
  for (std::size_t I = 1; I < Nodes.back()->Parameters.size(); ++I) {
    // There might be too many parameters.
    if (I > Def->ParameterTypes.size()) {
      auto &Diag = Diagnostics.emplace_back(*Nodes.back(), I);
      Diag.Kind = Diagnostic::Warning;
      Diag.Message = fmt::format("Unused parameter.");
      continue;
    }

    auto Type = Type::FromString(Nodes.back()->Parameters[I]);
    if (!Def->ParameterTypes[I - 1].VerifyType(Type,
                                               Nodes.back()->Parameters[I])) {
      auto &Diag = Diagnostics.emplace_back(*Nodes.back(), I);
      Diag.Kind = Diagnostic::Error;
      Diag.Message = fmt::format("Expected type '{}' got '{}'.",
                                 Def->ParameterTypes[I - 1], Type);
      continue;
    }
  }
  // There might be too few parameters.
  if (Optional >= Nodes.back()->Parameters.size()) {
    auto &Diag = Diagnostics.emplace_back(*Nodes.back(), 0);
    Diag.Kind = Diagnostic::Error;
    Diag.Message = fmt::format("Not enough arguments, {} missing.",
                               Optional - Nodes.back()->Parameters.size() + 1);
  }

  // Now we can assign the definition to the data node.
  Nodes.back()->Definition = Def;
}
