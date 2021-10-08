#include "definitions.h"

#include <cassert>
#include <unordered_map>

namespace {

const std::set<std::string_view> Roots{
    "color",     "conversation", "effect",     "event",
    "fleet",     "galaxy",       "government", "hazard",
    "interface", "minable",      "mission",    "outfit",
    "outfitter", "person",       "phrase",     "planet",
    "ship",      "shipyard",     "start",      "system",
    "test",      "test-data",    "trade",      "landing message",
    "star",      "news",         "rating",     "category",
    "tip",       "help"};

const std::unordered_map<std::string_view, std::set<std::string_view>> Children{
    {"color", {}},
    {"conversation",
     {"scene", "label", "choice", "name", "branch", "apply", "action"}},
    {"effect",
     {"sprite" /*full*/, "sound", "lifetime", "random lifetime",
      "velocity scale", "random velocity", "random angle", "random spin",
      "random frame rate"}},
    {"event",
     {"date", "unvisit", "visit", "unvisit planet", "visit planet", "link",
      "unlink", "fleet", "galaxy", "government", "outfitter", "news", "planet",
      "shipyard", "system"}},
    {"fleet",
     {"government", "names", "fighters", "cargo", "commodities", "outfitters",
      "personality", "variant"}},
    {"galaxy", {"pos", "sprite" /*simple*/}},
    {"government",
     {"display name", "swizzle", "color", "player reputation", "crew attack",
      "crew defense", "attitude toward", "penalty for", "bribe", "fine",
      "enforces", "death sentence", "friendly hail", "friendly disabled hail",
      "hostile hail", "hostile disabled hail", "language", "raid"}},
    {"hazard",
     {"weapon", "constant strength", "period", "duration", "strength", "range",
      "environmental effect"}},
    {"interface",
     {"anchor", "value", "point", "visible", "active", "sprite", "image",
      "outline", "label", "string", "button", "bar", "ring"}},
    {"minable", {"sprite" /*simple*/, "hull", "payload", "explode"}},
    {"mission",
     {"name",      "uuid",       "description", "blocked",     "deadline",
      "cargo",     "passengers", "illegal",     "stealth",     "invisible",
      "priority",  "minor",      "autosave",    "job",         "landing",
      "assisting", "boarding",   "repeat",      "clearance",   "infiltrating",
      "failed",    "to",         "source",      "destination", "waypoint",
      "stopover",  "npc",        "on"}},
    {"outfit",
     {"category",
      "plural",
      "flare sprite" /*full*/,
      "reverse flare sprite" /*full*/,
      "steering flare sprite" /*full*/,
      "flare sound",
      "reverse flare sound",
      "steering flare sound",
      "afterburner effect",
      "jump effect",
      "hyperdrive sound",
      "hyperdrive in sound",
      "hyperdrive out sound",
      "jump sound",
      "jump in sound",
      "jump out sound",
      "floatsam sprite" /*simple*/,
      "thumbnail" /*simple*/,
      "weapon",
      "ammo",
      "description",
      "cost",
      "mass",
      "licenses",
      "jump range"}},  // Infinite children
    {"outfitter", {}}, // Infinite children iff outfit
    {"person",
     {"system", "frequency", "ship", "government", "personality", "phrase"}},
    {"phrase", {}},
    {"planet",
     {"attributes", "shipyard", "outfitter", "landscape", "music",
      "description", "spaceport", "government", "required reputation", "bribe",
      "security", "tribute"}},
    {"ship",
     {"sprite" /*full*/,
      "thumbnail" /*simple*/,
      "name",
      "plural",
      "noun",
      "swizzle",
      "uuid",
      "attributes",
      "engine",
      "reverse engine",
      "steering engine",
      "gun",
      "turret",
      "never disabled",
      "uncapturable",
      "fighter",
      "drone",
      "bay",
      "leak",
      "explode",
      "final explode",
      "outfits",
      "cargo",
      "crew",
      "fuel",
      "shields",
      "hull",
      "position",
      "system",
      "planet",
      "destination system",
      "parked",
      "description",
      "actions"}},
    {"shipyard", {}}, // Infinite children iff ship.
    {"start",
     {"name", "description", "thumbnail" /*simple*/, "ship",
      "conversation"}}, // + unlimited conditions.
    {"system",
     {"hidden", "attributes", "link", "asteroids", "minables", "fleet",
      "hazard", "pos", "government", "music", "habitable", "belt", "jump range",
      "haze", "trade", "object", "arrival"}},
    {"test", {"key", "pointer", "command"}},
    {"test-data", {"category"}}, // + a save game.
    {"trade", {"commodity"}},
    {"landing message", {}}, // Infinite children iff sprite.
    {"star", {"power", "wind"}},
    {"news", {"location", "name", "portrait", "message", "to"}},
    {"rating", {}}, // FIXME
    {"category", {}},
    {"tip", {}},
    {"help", {}}, // FIXME
};

const std::unordered_map<std::string_view, std::vector<lsp::Type>> Types{
    {"color",
     {lsp::Type::String, lsp::Type::Double, lsp::Type::Double,
      lsp::Type::Double, lsp::Type::Double}},
    {"conversation", {lsp::Type::String}},
    {"effect", {lsp::Type::String}},
    {"event", {lsp::Type::String}},
    {"fleet", {lsp::Type::String}},
    {"galaxy", {lsp::Type::String}},
    {"government", {lsp::Type::String}},
    {"hazard", {lsp::Type::String}},
    {"interface", {lsp::Type::String}},
    {"minable", {lsp::Type::String}},
    {"mission", {lsp::Type::String}},
    {"outfit", {lsp::Type::String}},
    {"outfitter", {lsp::Type::String}},
    {"person", {lsp::Type::String}},
    {"phrase", {lsp::Type::OptionalString}},
    {"planet", {lsp::Type::String}},
    {"ship", {lsp::Type::String, lsp::Type::OptionalString}},
    {"shipyard", {lsp::Type::String}},
    {"start", {lsp::Type::OptionalString}},
    {"system", {lsp::Type::String}},
    {"test", {lsp::Type::String}},
    {"test-data", {lsp::Type::String}},
    {"trade", {lsp::Type::String}},
    {"landing message", {lsp::Type::String}},
    {"star", {lsp::Type::String}},
    {"news", {lsp::Type::String}},
    {"rating", {lsp::Type::String}},
    {"category", {lsp::Type::Keyword}},
    {"tip", {lsp::Type::String}},
    {"help", {lsp::Type::String}},
};

} // namespace

lsp::Type::Type(std::string_view String) noexcept {
  // Try to parse as a number. If it's not a number then it's a string.
  // [+-]?[0-9]*[.]?[0-9]*([eE][+-]?[0-9]*)?
  Kind = Type::String;
  if (String.empty())
    return;
  std::size_t I = 0;

  if (String[I] == '+' || String[I] == '-')
    ++I;
  while (I < String.size() && String[I] >= '0' && String[I] <= '9')
    ++I;
  if (String[I] == '.')
    ++I;
  while (I < String.size() && String[I] >= '0' && String[I] <= '9')
    ++I;
  if (String[I] == 'e' || String[I] == 'E') {
    if (String[I] == '+' || String[I] == '-')
      ++I;
    while (I < String.size() && String[I] >= '0' && String[I] <= '9')
      ++I;
  }

  if (I == String.size())
    Kind = Type::Double;
}

lsp::Type::operator std::string_view() const noexcept {
  switch (Kind) {
  case Type::Keyword:
    return "keyword";
  case Type::Double:
  case Type::OptionalDouble:
    return "double";
  case Type::String:
  case Type::OptionalString:
    return "string";
  default:
    __builtin_unreachable();
  }
}

bool lsp::IsValidRootNode(std::string_view Name) { return Roots.count(Name); }

const std::set<std::string_view> &lsp::GetChildrenOf(std::string_view Root) {
  auto It = Children.find(Root);
  assert(It != Children.end() && "invalid root name");
  return It->second;
}

const std::vector<lsp::Type> &lsp::GetParameterTypesOf(std::string_view First) {
  auto It = Types.find(First);
  assert(It != Types.end() && "invalid root name");
  return It->second;
}
