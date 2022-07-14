import { BrowserRouter, Routes, Route } from "react-router-dom";
import Home from "./Home";
import Game from "./Game";
import AddGame from "./AddGame";
import Navbar from "./components/Navbar";

export default function App() {
  return (
    <BrowserRouter>
      <Navbar />
      <Routes>
        <Route path="/" element={<Home />} />
        <Route path="/add-game" element={<AddGame />} />
        <Route path="game/:gameId/:username" element={<Game />} />
      </Routes>
    </BrowserRouter>
  );
}
