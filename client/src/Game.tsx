import { useEffect, useState } from "react";
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
    <div>
      <div>
        <h2>Game</h2>
        <Link to="/">Home</Link>
      </div>

      {state.phase === "idle" && <div>Connecting...</div>}
      {state.phase === "waiting" && (
        <div>
          <h4>Waiting for players to connect</h4>
          <h4>{state.users.size}/3</h4>
        </div>
      )}
      {state.phase === "error" && (
        <div>GO BACK TO HOME. ERROR HAPPENED WTF!!!!!</div>
      )}
      {state.phase === "started" && (
        <GameArea game={state.game} username={username} onGuess={onGuess} />
      )}
      {state.phase === "ended" && (
        <div>
          <h2>Game ended</h2>
          <p>{state.winner} won the game.</p>
          <p>correct answer was number {state.answer}.</p>
        </div>
      )}
    </div>
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
    <div>
      <div>
        <h2>Game ID: {game.id}</h2>
        <h2>Username: {username}</h2>
      </div>
      <div>
        {isYourTurn && <p>Its your turn</p>}
        {!isYourTurn && <p>Waiting for {game.turn} to guess.</p>}

        <input
          type="number"
          value={guess}
          onChange={(ev) => setGuess(parseInt(ev.target.value))}
        />
        <button onClick={() => onGuess(guess)}>Guess</button>
      </div>
      <div>
        <h3>Players</h3>
        {game.players.map((player) => (
          <div key={player}>{player}</div>
        ))}
      </div>

      <div>
        <h3>Guess Log</h3>
        {game.guessesLog.map((guess) => (
          <div key={guess.number}>
            user {guess.username} guessed {guess.number}
          </div>
        ))}
      </div>
    </div>
  );
}

export default Game;
