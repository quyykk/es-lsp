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
    {"scene", {lsp::Type::Sprite}},
    {"label", {lsp::Type::String}},
    {"choice", {}},
    {"name", {}},
    {"branch", {}},
    {"action", {}},
    {"apply", {}},
    {"effect", {lsp::Type::String}},
    {"sprite", {lsp::Type::Sprite}},
    {"lifetime", {lsp::Type::Double}},
    {"random lifetime", {lsp::Type::Double}},
    {"velocity scale", {lsp::Type::Double}},
    {"random velocity", {lsp::Type::Double}},
    {"random angle", {lsp::Type::Double}},
    {"random spin", {lsp::Type::Double}},
    {"random frame rate", {lsp::Type::Double}},
    {"event", {lsp::Type::String}},
    {"date", {lsp::Type::Double, lsp::Type::Double, lsp::Type::Double}},
    {"unvisit", {lsp::Type::System}},
    {"visit", {lsp::Type::System}},
    {"unvisit planet", {lsp::Type::Planet}},
    {"visit planet", {lsp::Type::Planet}},
    {"fleet", {lsp::Type::Fleet}},
    {"names", {lsp::Type::String}}, // FIXME
    {"fighters", {lsp::Type::String}}, // FIXME
    {"cargo", {lsp::Type::Double}},
    {"commodities", {}},
    {"outfitters", {}},
    {"personality", {lsp::Type::OptionalString, lsp::Type::OptionalString}}, // FIXME
    {"variant", {lsp::Type::String}},
    {"galaxy", {lsp::Type::String}},
    {"pos", {lsp::Type::Double, lsp::Type::Double}},
    {"government", {lsp::Type::String}},
    {"display name", {lsp::Type::String}},
    {"swizzle", {lsp::Type::Double}},
    {"player reputation", {lsp::Type::Double}},
    {"crew attack", {lsp::Type::Double}},
    {"crew defense", {lsp::Type::Double}},
    {"attitude toward", {}},
    {"penalty for", {}},
    {"bribe", {lsp::Type::Double}},
    {"fine", {lsp::Type::Double}},
    {"enforces", {}},
    {"death sentence", {lsp::Type::String}}, // FIXME
    {"friendly hail", {lsp::Type::String}}, // FIXME
    {"friendly disabled hail", {lsp::Type::String}}, // FIXME
    {"hostile hail", {lsp::Type::String}}, // FIXME
    {"hostile disabled hail", {lsp::Type::String}}, // FIXME
    {"language", {lsp::Type::String}},
    {"raid", {lsp::Type::Fleet}},
    {"hazard", {lsp::Type::String}},
    {"constant strength", {}},
    {"period", {lsp::Type::Double}},
    {"duration", {lsp::Type::Double, lsp::Type::OptionalDouble}},
    {"strength", {lsp::Type::Double, lsp::Type::OptionalDouble}},
    {"range", {lsp::Type::Double, lsp::Type::OptionalDouble}},
    {"environmental effect", {lsp::Type::Effect, lsp::Type::OptionalDouble}},
    {"interface", {lsp::Type::String}},
    {"anchor", {lsp::Type::String}}, // FIXME
    {"value", {lsp::Type::String, lsp::Type::Double}},
    {"point", {lsp::Type::String}},
    {"box", {lsp::Type::String}},
    {"visible", {lsp::Type::String, lsp::Type::OptionalString}}, // FIXME
    {"active", {lsp::Type::String, lsp::Type::OptionalString}}, // FIXME
    // FIXME: Fix interface duplicate nodes with different behavior than expected.
    {"minable", {lsp::Type::String}},
    {"hull", {lsp::Type::Double}},
    {"payload", {lsp::Type::String, lsp::Type::OptionalDouble}},
    {"explode", {lsp::Type::String, lsp::Type::OptionalDouble}},
    {"mission", {lsp::Type::String}},
    {"name", {lsp::Type::String}},
    {"uuid", {lsp::Type::String}}, // FIXME?
    {"description", {lsp::Type::String}},
    {"blocked", {lsp::Type::String}},
    {"deadline", {lsp::Type::Double, lsp::Type::Double, lsp::Type::Double}},
    {"cargo", {lsp::Type::Double, lsp::Type::Double, lsp::Type::OptionalDouble, lsp::Type::OptionalDouble}},
    {"passengers", {lsp::Type::Double, lsp::Type::OptionalDouble, lsp::Type::OptionalDouble}},
    {"illegal", {lsp::Type::Double, lsp::Type::OptionalString}},
    {"stealth", {}},
    {"invisible", {}},
    {"priority", {}},
    {"minor", {}},
    {"autosave", {}},
    {"job", {}},
    {"landing", {}},
    {"assisting", {}},
    {"boarding", {}},
    {"repeat", {lsp::Type::OptionalDouble}},
    {"clearance", {lsp::Type::OptionalDouble}},
    {"infiltrating", {}},
    {"failed", {}},
    {"to", {lsp::Type::String}}, // FIXME
    {"source", {lsp::Type::OptionalString}}, // FIXME
    {"destination", {lsp::Type::OptionalString}}, // FIXME
    {"waypoint", {lsp::Type::OptionalString, lsp::Type::OptionalString}}, // FIXME
    {"stopover", {lsp::Type::OptionalString, lsp::Type::OptionalString}}, // FIXME
    {"npc", {}},
    {"on", {lsp::Type::String}}, // FIXME
    {"outfit", {lsp::Type::String}},
    {"category", {lsp::Type::String}},
    {"plural", {lsp::Type::String}},
    {"flare sprite", {lsp::Type::Sprite}},
    {"reverse flare sprite", {lsp::Type::Sprite}},
    {"steering flare sprite", {lsp::Type::Sprite}},
    {"flare sound", {lsp::Type::String}}, // FIXME
    {"reverse flare sound", {lsp::Type::String}}, // FIXME
    {"steering flare sound", {lsp::Type::String}}, // FIXME
    {"afterburner effect", {lsp::Type::Effect}},
    {"jump effect", {lsp::Type::Effect}},
    {"hyperdrive sound", {lsp::Type::String}}, // FIXME
    {"hyperdrive in sound", {lsp::Type::String}}, // FIXME
    {"hyperdrive out sound", {lsp::Type::String}}, // FIXME
    {"jump sound", {lsp::Type::String}}, // FIXME
    {"jump in sound", {lsp::Type::String}}, // FIXME
    {"jump out sound", {lsp::Type::String}}, // FIXME
    {"floatsam sprite", {lsp::Type::Sprite}},
    {"thumbnail", {lsp::Type::Sprite}},
    {"weapon", {}},
    {"ammo", {lsp::Type::Outfit}},
    {"cost", {lsp::Type::Double}},
    {"mass", {lsp::Type::Double}},
    {"licenses", {lsp::Type::OptionalString, lsp::Type::OptionalString, lsp::Type::OptionalString}}, // FIXME
    {"jump range", {lsp::Type::Double}},
    {"outfitter", {lsp::Type::String}},
    {"person", {lsp::Type::String}},
    // {"system", {}}, // FIXME
    {"frequency", {lsp::Type::Double}},
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
    // FIXME: {"category", {lsp::Type::Keyword}},
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
  case Type::Effect:
    return "effect";
  case Type::Fleet:
    return "fleet";
  case Type::Hazard:
    return "hazard";
  case Type::Outfit:
    return "outfit";
  case Type::Planet:
    return "planet";
  case Type::Ship:
    return "ship";
  case Type::Sprite:
    return "sprite";
  case Type::System:
    return "system";
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

const std::vector<lsp::Type> *lsp::GetParameterTypesOf(std::string_view First) {
  auto It = Types.find(First);
  if (It == Types.end())
      return nullptr;
  return &It->second;
}
