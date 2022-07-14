import {
  Box,
  Button,
  Flex,
  FormControl,
  FormLabel,
  Heading,
  Input,
} from "@chakra-ui/react";
import { useState } from "react";
import { z } from "zod";
import { useNavigate } from "react-router-dom";

function AddGame() {
  const [maxPlayers, setMaxPlayers] = useState(0);
  let navigate = useNavigate();

  function onSubmit() {
    fetch("http://localhost:5000/games", { method: "POST" })
      .then((res) => res.json())
      .then((res) => z.object({ gameId: z.string() }).parse(res))
      .then((res) => {
        navigate("/");
        setMaxPlayers(0);
      });
  }

  return (
    <Flex justifyContent="center" alignItems="center" flexDirection="column">
      <Box width="50%">
        <Box
          display="flex"
          flexDirection="row"
          justifyContent="space-between"
          marginBottom="20px"
        >
          <Heading color="white" data-testid="header">
            Add Game
          </Heading>
        </Box>
        <FormLabel color="white">Max players</FormLabel>
        <Input
          value={maxPlayers}
          color="white"
          placeholder="Number of players needed to get started"
          type="number"
          onChange={(e) => setMaxPlayers(parseInt(e.currentTarget.value))}
        />
        <Button
          marginTop={4}
          colorScheme="teal"
          type="submit"
          onClick={onSubmit}
        >
          Submit
        </Button>
      </Box>
    </Flex>
  );
}

export default AddGame;
