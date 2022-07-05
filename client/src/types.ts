import { z } from "zod";

export type GameId = string;

export type Username = string;

export const Room = z.object({
  maxClients: z.number(),
  clientsCount: z.number(),
  roomId: z.string(),
});

export type Room = z.infer<typeof Room>;

export const Guess = z.object({
  number: z.number(),
  username: z.string(),
  gameId: z.string(),
});

export type Guess = z.infer<typeof Guess>;

export const Game = z.object({
  id: z.string(),
  players: z.array(z.string()),
  turn: z.string(),
  guessesLog: z.array(Guess),
});

export type Game = z.infer<typeof Game>;

export const GameStarted = z.object({
  tag: z.literal("GameStarted"),
  contents: Game,
});

export const JoinedTo = z.object({
  tag: z.literal("JoinedTo"),
  contents: z.array(z.string()),
});

export const UserJoined = z.object({
  tag: z.literal("UserJoined"),
  contents: z.string(),
});

export const GuessResponse = z.object({
  isCorrect: z.boolean(),
  number: z.number(),
  username: z.string(),
  nextTurn: z.string(),
});

export type GuessResponse = z.infer<typeof GuessResponse>;

export const Guessed = z.object({
  tag: z.literal("Guessed"),
  contents: GuessResponse,
});

export const GameEnded = z.object({
  tag: z.literal("GameEnded"),
  contents: z.tuple([z.string(), z.number()]),
});

export const Message = z.discriminatedUnion("tag", [
  GameStarted,
  JoinedTo,
  UserJoined,
  Guessed,
  GameEnded,
]);

export type Message = z.infer<typeof Message>;

export const GameInfo = z.object({
  gameId: z.string(),
});

export type GameInfo = z.infer<typeof GameInfo>;
