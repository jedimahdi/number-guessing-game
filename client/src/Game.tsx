import { useEffect, useState } from "react";
import {
  Box,
  Button,
  Flex,
  FormControl,
  FormLabel,
  Heading,
  Input,
  Text,
} from "@chakra-ui/react";
import { useParams, useNavigate, Link } from "react-router-dom";
import useWebSocket from "react-use-websocket";
import { Message } from "./types";
import * as t from "./types";

type State =
  | { phase: "idle" }
  | { phase: "waiting"; users: Set<t.Username> }
  | { phase: "started"; game: t.Game }
  | { phase: "error" }
  | { phase: "ended"; winner: string; answer: number };

function Game() {
  const [state, setState] = useState<State>({ phase: "idle" });

  const params = useParams();
  const username = params.username || "";
  const gameId = params.gameId || "";

  const navigate = useNavigate();
  const { sendJsonMessage, getWebSocket } = useWebSocket(
    `ws://localhost:5000/socket/${gameId}/${username}`,
    {
      onOpen: () => console.log("WebSocket connection opened."),
      onClose: () => console.log("WebSocket connection closed."),
      onError: (e) => {
        console.log({ error: e });
        setState({ phase: "error" });
      },
      shouldReconnect: (closeEvent) => true,
      onMessage: (event: WebSocketEventMap["message"]) =>
        processMessages(event),
    }
  );

  function processMessages(event: any) {
    console.log({ data: JSON.parse(event.data) });
    const message = Message.parse(JSON.parse(event.data));
    console.log(message);
    switch (message.tag) {
      case "JoinedTo":
        setState({
          phase: "waiting",
          users: new Set(message.contents).add(username),
        });
        break;
      case "UserJoined":
        if (state.phase === "waiting") {
          setState({ ...state, users: state.users.add(message.contents) });
        }
        break;
      case "Guessed":
        if (state.phase === "started") {
          setState({
            ...state,
            game: {
              ...state.game,
              turn: message.contents.nextTurn,
              guessesLog: [
                {
                  number: message.contents.number,
                  username: message.contents.username,
                  gameId,
                },
                ...state.game.guessesLog,
              ],
            },
          });
        }
        break;
      case "GameStarted":
        setState({ phase: "started", game: message.contents });
        break;
      case "GameEnded":
        setState({
          phase: "ended",
          winner: message.contents[0],
          answer: message.contents[1],
        });
        break;
    }
  }

  function onGuess(n: number) {
    fetch("http://localhost:5000/game/guess", {
      method: "POST",
      body: JSON.stringify({ number: n, username, gameId }),
      headers: {
        "Content-Type": "application/json",
      },
    })
      .then((res) => res.json())
      .then((res) => console.log(res));
  }

  return (
    <Flex
      justifyContent="center"
      alignItems="center"
      flexDirection="column"
      color="white"
    >
      <Box width="50%"></Box>
      <Box>
        <Heading>Game</Heading>
        <Link to="/">Home</Link>
      </Box>

      {state.phase === "idle" && <Box>Connecting...</Box>}
      {state.phase === "waiting" && (
        <Box>
          <Heading>Waiting for players to connect</Heading>
          <Heading>{state.users.size}/3</Heading>
        </Box>
      )}
      {state.phase === "error" && (
        <Box>GO BACK TO HOME. ERROR HAPPENED WTF!!!!!</Box>
      )}
      {state.phase === "started" && (
        <GameArea game={state.game} username={username} onGuess={onGuess} />
      )}
      {state.phase === "ended" && (
        <Box>
          <Heading>Game ended</Heading>
          <Text>{state.winner} won the game.</Text>
          <Text>correct answer was number {state.answer}.</Text>
        </Box>
      )}
    </Flex>
  );
}

function GameArea({
  game,
  username,
  onGuess,
}: {
  game: t.Game;
  username: string;
  onGuess: (n: number) => void;
}) {
  const [guess, setGuess] = useState(0);

  const isYourTurn = username === game.turn;

  return (
    <Box display="flex" flexDirection="column">
      <Heading color="white">Game {game.id}</Heading>
      <Heading color="white">Username: {username}</Heading>

      <Box>
        {isYourTurn && <p>Its your turn</p>}
        {!isYourTurn && <p>Waiting for {game.turn} to guess.</p>}

        <Input
          type="number"
          value={guess}
          onChange={(ev) => setGuess(parseInt(ev.target.value))}
        />
        <Button variant="outline" onClick={() => onGuess(guess)}>
          Guess
        </Button>
      </Box>

      <Box>
        <Heading>Players</Heading>
        {game.players.map((player) => (
          <Box key={player}>{player}</Box>
        ))}
      </Box>

      <Box>
        <Heading>Guess Log</Heading>
        {game.guessesLog.map((guess) => (
          <Box key={guess.number}>
            user {guess.username} guessed {guess.number}
          </Box>
        ))}
      </Box>
    </Box>
  );
}

export default Game;
