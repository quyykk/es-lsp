#include "catch.hpp"

#include <string_view>

#include "DataNode.h"
#include "lsp.h"

TEST_CASE("DataNodes are parsed and checked correctly", "[datanodes]") {
  std::string_view Text = R"(# jdlkajf
   # Tyaya # fjfjjf###
        ####

        
hazard R #fla fljdkjfaljf fnj ###
	period    3

	#jfladjf
effect "FFFF"
	"random velocity"  -1.23455	#flajf
effect rt
	###fjl "F
)";

  const auto Root = lsp::LoadFromText("some path", Text);

  REQUIRE(Root.Nodes.size() == 5);
  REQUIRE(Root.Nodes.find(5) != Root.Nodes.end());
  const auto &Five = Root.Nodes.at(5);
  REQUIRE(Five.Parameters.size() == 2);
  CHECK(Five.Parameters[0] == "hazard");
  CHECK(Five.Parameters[1] == "R");
  REQUIRE(Five.Quoted.size() == 2);
  CHECK_FALSE(Five.Quoted[0]);
  CHECK_FALSE(Five.Quoted[1]);
  CHECK_FALSE(Five.Parent);
  CHECK(Five.Line == 5);
  REQUIRE(Five.Columns.size() == 2);
  CHECK(Five.Columns[0] == 0);
  CHECK(Five.Columns[1] == 7);
  REQUIRE(Five.Children.size() == 1);

  REQUIRE(Root.Diagnostics.empty());

  REQUIRE(Root.Entities.size() == 2);
  REQUIRE(Root.Entities.at("hazard").size() == 1);
  REQUIRE(Root.Entities.at("effect").size() == 2);
  REQUIRE(Root.Entities.at("hazard").back() == "R");
  REQUIRE(Root.Entities.at("effect")[0] == "FFFF");
  REQUIRE(Root.Entities.at("effect")[1] == "rt");

  REQUIRE(Root.Path == "some path");
}

