import { useEffect, useState } from "react";
import { Room } from "./types";
import { z } from "zod";
import { Link, useNavigate } from "react-router-dom";

import { ChevronRightIcon } from "@chakra-ui/icons";
import {
  Heading,
  Flex,
  Text,
  Button,
  Box,
  Stack,
  IconButton,
  Input,
  FormLabel,
} from "@chakra-ui/react";

function Home() {
  const [rooms, setRooms] = useState<Room[]>([]);
  const [username, setUsername] = useState(makeid(5));

  const navigate = useNavigate();

  useEffect(() => {
    fetchRooms();
  }, []);

  function fetchRooms() {
    fetch("http://localhost:5000/rooms")
      .then((res) => res.json())
      .then((res) => z.array(Room).parse(res))
      .then((rooms) => {
        setRooms(rooms);
        console.log({ rooms });
      });
  }

  function joinRoom(roomId: string) {
    navigate(`/game/${roomId}/${username}`);
  }

  return (
    <Flex justifyContent="center" alignItems="center" flexDirection="column">
      <Box width="50%">
        <FormLabel color="white">Username</FormLabel>
        <Input
          value={username}
          color="white"
          placeholder="Username"
          onChange={(ev) => setUsername(ev.target.value)}
        />
      </Box>
      <Box width="50%" marginTop="25">
        <Box
          display="flex"
          flexDirection="row"
          justifyContent="space-between"
          marginBottom="20px"
        >
          <Heading color="white">Lobby</Heading>
          <Button paddingX="3rem" onClick={fetchRooms}>
            Refresh
          </Button>
        </Box>

        <Box color="white" px="15px" py="15px">
          <Stack spacing={8}>
            {rooms.map((room) => (
              <Box
                key={room.roomId}
                p={5}
                justifyContent="space-between"
                display="flex"
                shadow="md"
                bg="blue.800"
                rounded="md"
                alignItems="center"
              >
                <Box
                  display="flex"
                  flexDirection="row"
                  alignItems="center"
                  justifyContent="center"
                >
                  <Heading fontSize="xl">{room.roomId} </Heading>

                  <Text mt="1" pl="2">
                    {room.clientsCount}/{room.maxClients}
                  </Text>
                </Box>
                <IconButton
                  display="block"
                  color="white"
                  bg="transparent"
                  aria-label=""
                  icon={<ChevronRightIcon />}
                  onClick={() => joinRoom(room.roomId)}
                />
              </Box>
            ))}
          </Stack>
        </Box>
      </Box>
    </Flex>
  );
}

function makeid(length: number) {
  var result = "";
  var characters = "abcdefghijklmnopqrstuvwxyz";
  var charactersLength = characters.length;
  for (var i = 0; i < length; i++) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
  }
  return result;
}

export default Home;
