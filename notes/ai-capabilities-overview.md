# KI-Fähigkeiten: Ein umfassender Überblick (Stand: Frühjahr 2025)

## Inhaltsverzeichnis

1. [Large Language Models (LLMs)](#1-large-language-models-llms)
2. [KI-Agents](#2-ki-agents) ← Schwerpunkt
3. [Code-Generierung & Software Engineering](#3-code-generierung--software-engineering)
4. [Multimodale KI](#4-multimodale-ki)
5. [Bild- und Videogenerierung](#5-bild--und-videogenerierung)
6. [Sprache und Audio](#6-sprache-und-audio)
7. [Wissenschaftliche KI](#7-wissenschaftliche-ki)
8. [Robotik und Embodied AI](#8-robotik-und-embodied-ai)
9. [Grenzen und offene Probleme](#9-grenzen-und-offene-probleme)

---

## 1. Large Language Models (LLMs)

### Was sie können

- **Textverständnis und -generierung**: Zusammenfassen, Übersetzen, Umschreiben, Analysieren von Texten in dutzenden Sprachen
- **Reasoning**: Mehrstufige logische Schlussfolgerungen, mathematische Beweise, Code-Analyse
- **Kontextfenster**: Moderne Modelle verarbeiten 100k–2M Tokens (~75k–1.5M Wörter) in einem Durchgang
- **Instruktionsbefolgung**: Komplexe, mehrstufige Anweisungen mit Einschränkungen und Formatvorgaben
- **Few-Shot und Zero-Shot Learning**: Aufgaben lösen mit wenigen oder keinen Beispielen

### Wichtige Modelle (Stand Frühjahr 2025)

| Anbieter | Modell | Besonderheit |
|----------|--------|--------------|
| Anthropic | Claude Opus 4, Sonnet 4 | Extended Thinking, Tool Use, Agents |
| OpenAI | GPT-4o, o1, o3 | Multimodal, Chain-of-Thought Reasoning |
| Google | Gemini 2.0, 2.5 | Langes Kontextfenster (bis 2M Tokens) |
| Meta | Llama 3.1 (405B) | Open Source, breite Community |
| Mistral | Mistral Large, Mixtral | Europäischer Anbieter, MoE-Architektur |
| DeepSeek | DeepSeek-V3, R1 | Open Source, starke Reasoning-Fähigkeiten |

### Trainingsparadigmen

- **Pre-Training**: Unsupervised auf riesigen Textkorpora (Billionen von Tokens)
- **Supervised Fine-Tuning (SFT)**: Menschliche Demonstration von erwünschtem Verhalten
- **RLHF / RLAIF**: Reinforcement Learning aus menschlichem / KI-Feedback
- **Constitutional AI**: Selbstkorrektur durch internalisierte Prinzipien (Anthropic)

---

## 2. KI-Agents

> **Besonderer Schwerpunkt** — da gerade selbst Agents parallel laufen

### Was ist ein KI-Agent?

Ein Agent ist ein KI-System, das **autonom mehrstufige Aufgaben** ausführt, indem es:
1. Ein Ziel interpretiert
2. Einen Plan erstellt
3. Tools aufruft (APIs, Code-Ausführung, Dateisystem, Browser...)
4. Zwischenergebnisse bewertet
5. Den Plan bei Bedarf anpasst
6. Bis zur Zielerreichung iteriert

Der fundamentale Unterschied zu einem einfachen LLM-Chat: **Agents handeln, statt nur zu antworten.**

### Architektur-Patterns

#### ReAct (Reason + Act)
```
Thought → Action → Observation → Thought → Action → ...
```
Das Modell denkt laut nach, führt eine Aktion aus, beobachtet das Ergebnis und entscheidet über den nächsten Schritt. Grundlegendes Pattern für fast alle aktuellen Agent-Systeme.

#### Plan-and-Execute
```
Plan erstellen → Schritte sequentiell abarbeiten → Plan bei Bedarf revidieren
```
Trennung von Planung und Ausführung. Besser für komplexe, langfristige Aufgaben.

#### Multi-Agent-Systeme
```
Orchestrator-Agent → delegiert an spezialisierte Sub-Agents
                   → aggregiert Ergebnisse
```
Mehrere Agents mit unterschiedlichen Rollen arbeiten zusammen. Genau das passiert hier gerade — parallele Agents für verschiedene Teilaufgaben.

### Tool Use (Function Calling)

Das Herzstück moderner Agents. Das LLM entscheidet **welches Tool** mit **welchen Parametern** aufgerufen wird:

- **Dateisystem**: Lesen, Schreiben, Suchen von Dateien
- **Code-Ausführung**: Bash, Python, beliebige Shells
- **Web**: HTTP-Requests, Browser-Automation, Web-Suche
- **APIs**: GitHub, Jira, Datenbanken, Cloud-Services
- **Spezialisierte Tools**: Compiler, Linter, Test-Frameworks

### Konkrete Agent-Frameworks & -Produkte

| System | Beschreibung |
|--------|-------------|
| **Claude Code (Anthropic)** | CLI-Agent für Software-Engineering — läuft gerade hier. Kann Dateien lesen/schreiben, Code ausführen, Git-Operationen, parallele Sub-Agents spawnen |
| **Claude Agent SDK** | SDK zum Bauen eigener Agents auf Claude-Basis |
| **OpenAI Agents SDK** | OpenAI's Framework für Custom Agents mit Handoffs und Guardrails |
| **Codex CLI (OpenAI)** | Terminal-Agent für Code-Aufgaben, ähnlich Claude Code |
| **Devin (Cognition)** | Autonomer Software-Entwickler-Agent mit eigener Sandbox |
| **Cursor / Windsurf** | IDE-integrierte Coding-Agents |
| **LangChain / LangGraph** | Open-Source Framework für Agent-Orchestrierung in Python/JS |
| **CrewAI** | Multi-Agent-Framework mit Rollen-basiertem Design |
| **AutoGen (Microsoft)** | Framework für konversationale Multi-Agent-Systeme |
| **Semantic Kernel** | Microsofts Enterprise-Agent-Framework |

### Parallele Agents (was hier gerade passiert)

Moderne Agent-Systeme können **mehrere Sub-Agents gleichzeitig** starten:

```
Haupt-Agent (Orchestrator)
├── Agent 1: Repository-Exploration    ← parallel
├── Agent 2: Git-Status prüfen         ← parallel
├── Agent 3: Web-Recherche             ← parallel
└── Agent 4: Datei-Analyse             ← parallel
         ↓
    Ergebnisse zusammenführen
         ↓
    Nächste Aktion entscheiden
```

**Vorteile**:
- Drastische Beschleunigung bei unabhängigen Teilaufgaben
- Jeder Agent hat eigenen Kontext und Spezialisierung
- Orchestrator muss nur die Ergebnisse aggregieren

**Herausforderungen**:
- Koordination: Agents dürfen nicht dieselbe Datei gleichzeitig ändern
- Kontext-Weitergabe: Sub-Agents kennen nicht automatisch die Ergebnisse der anderen
- Kosten: Jeder Agent verbraucht eigene Token-Kontingente
- Fehlerbehandlung: Was passiert, wenn ein Sub-Agent fehlschlägt?

### Agentic Coding: Der aktuelle Stand

KI-Agents können heute autonom:
- **Bugs fixen**: Issue lesen → Code analysieren → Fix implementieren → Tests schreiben → PR erstellen
- **Features implementieren**: Anforderung verstehen → Plan erstellen → über mehrere Dateien hinweg implementieren
- **Refactoring**: Codebase analysieren → Verbesserungen identifizieren → schrittweise umbauen
- **CI/CD-Fehler beheben**: Build-Logs lesen → Problem diagnostizieren → Fix anwenden
- **Code-Reviews**: Änderungen analysieren → Probleme identifizieren → Verbesserungen vorschlagen

### Agent-Erinnerung und Zustand

- **Kurzzeit**: Konversationskontext (begrenzt durch Token-Fenster)
- **Mittelzeit**: Tool-Aufrufe und deren Ergebnisse innerhalb einer Session
- **Langzeit**: Projektdateien (CLAUDE.md, Memory-Files), die zwischen Sessions bestehen bleiben
- **RAG (Retrieval-Augmented Generation)**: Dynamisches Abrufen relevanter Informationen aus Vektordatenbanken

### Computer Use & Browser Agents

Agents, die direkt mit grafischen Oberflächen interagieren:
- **Claude Computer Use**: Screenshots interpretieren, Maus und Tastatur steuern
- **Browser Use**: Web-Formulare ausfüllen, durch Websites navigieren, Daten extrahieren
- **Desktop Automation**: Anwendungen öffnen, Menüs bedienen, Workflow-Automatisierung

---

## 3. Code-Generierung & Software Engineering

### Fähigkeiten

- **Code-Vervollständigung**: Inline-Vorschläge während des Tippens (Copilot, Supermaven, Codeium)
- **Code-Generierung**: Ganze Funktionen, Klassen, Module aus natürlichsprachlichen Beschreibungen
- **Code-Erklärung**: Bestehenden Code analysieren und verständlich erklären
- **Debugging**: Fehler identifizieren, Ursachenanalyse, Fix-Vorschläge
- **Test-Generierung**: Unit-Tests, Integration-Tests, Property-Based Tests
- **Dokumentation**: Docstrings, README-Dateien, API-Dokumentation
- **Übersetzung**: Code zwischen Programmiersprachen portieren

### Unterstützte Sprachen (praktisch alle gängigen)

Haskell, Python, JavaScript/TypeScript, Rust, Go, Java, C/C++, C#, Ruby, Swift, Kotlin, Scala, Elixir, Clojure, Lua, R, SQL, Shell/Bash, und viele mehr.

### Formale Verifikation mit KI

- LLMs können bei der Erstellung formaler Beweise assistieren (Lean 4, Coq, Isabelle)
- Automatische Generierung von Proof-Sketches
- Noch stark auf menschliche Führung angewiesen

---

## 4. Multimodale KI

### Vision (Bildverständnis)

- **Bildbeschreibung**: Detaillierte Analyse von Fotos, Diagrammen, Screenshots
- **OCR**: Text aus Bildern extrahieren, auch handschriftlich
- **Dokument-Analyse**: PDFs, Formulare, Tabellen, Graphen interpretieren
- **Visuelle Frage-Antwort**: "Was zeigt dieses Diagramm?" → strukturierte Antwort
- **UI-Verständnis**: Screenshots von Anwendungen interpretieren für Computer Use

### Audio-Verständnis

- Transkription von Sprache (Whisper, Gemini)
- Erkennung von Emotionen, Tonfall, Sprecherwechsel
- Musik-Analyse und -Beschreibung

### Video-Verständnis

- Gemini 2.0 kann Videos direkt verarbeiten
- Szenen beschreiben, Handlungen erkennen, zeitliche Zusammenhänge verstehen

---

## 5. Bild- und Videogenerierung

### Bildgenerierung

| System | Besonderheit |
|--------|-------------|
| DALL-E 3 | In ChatGPT integriert, gute Textdarstellung |
| Midjourney v6 | Hohe ästhetische Qualität |
| Stable Diffusion 3 / SDXL | Open Source, lokal ausführbar |
| Flux (Black Forest Labs) | Neue Architektur, sehr detailliert |
| Imagen 3 (Google) | Photorealismus |
| Ideogram 2.0 | Exzellente Typografie in Bildern |

### Videogenerierung

| System | Besonderheit |
|--------|-------------|
| Sora (OpenAI) | Physikalisch kohärente Videos |
| Runway Gen-3 | Professionelle Video-Bearbeitung |
| Kling (Kuaishou) | Lange, konsistente Clips |
| Veo 2 (Google) | 4K-Videos mit hoher Konsistenz |
| Wan (Alibaba) | Open Source Video-Modell |

### Fähigkeiten

- Text-zu-Bild / Text-zu-Video
- Bild-zu-Video (Animierung von Standbildern)
- Inpainting / Outpainting (Teile ersetzen / erweitern)
- Style Transfer
- 3D-Asset-Generierung aus 2D-Bildern

---

## 6. Sprache und Audio

### Text-to-Speech (TTS)

- **ElevenLabs**: Hochrealistische Stimmen, Voice Cloning
- **OpenAI TTS**: Natürliche Stimmen, in API verfügbar
- **Bark (Suno)**: Open Source, multilingual
- **XTTS (Coqui)**: Open Source Voice Cloning

### Speech-to-Text (STT)

- **Whisper (OpenAI)**: Open Source, extrem robust, 100+ Sprachen
- **Deepgram**: Echtzeit-Transkription, optimiert für Produktion
- **AssemblyAI**: Speaker Diarization, Sentiment-Analyse

### Musik-Generierung

- **Suno**: Songs mit Gesang aus Textbeschreibung
- **Udio**: Ähnlich, hohe Audioqualität
- **MusicLM / MusicFX (Google)**: Instrumentalmusik
- **Stable Audio**: Open Source Ansatz

### Echtzeit-Konversation

- OpenAI Realtime API: Sprach-zu-Sprach mit niedriger Latenz
- Gemini Live: Multimodale Echtzeit-Konversation
- Ermöglicht natürliche Dialoge mit Unterbrechungen und Emotionserkennung

---

## 7. Wissenschaftliche KI

### Protein-Struktur & Biologie

- **AlphaFold 2/3 (DeepMind)**: Proteinstruktur-Vorhersage — revolutionierte die Strukturbiologie
- **ESMFold (Meta)**: Schnellere Proteinstruktur-Vorhersage
- **RFdiffusion**: Protein-Design (neue Proteine generieren)

### Mathematik

- **AlphaProof / AlphaGeometry (DeepMind)**: Mathematische Beweise auf Olympiade-Niveau
- **LLMs als Mathematik-Assistenten**: Beweise skizzieren, Vermutungen prüfen, formalisieren
- Noch weit entfernt von autonomer mathematischer Forschung

### Materialwissenschaft & Chemie

- **GNoME (DeepMind)**: Entdeckung von 2.2 Millionen neuen Kristallstrukturen
- **Molekül-Design**: Generierung neuer Wirkstoffkandidaten
- **Reaktionsvorhersage**: Chemische Reaktionen modellieren

### Wetter & Klima

- **GraphCast (DeepMind)**: 10-Tage-Wettervorhersage, schneller und genauer als traditionelle Modelle
- **Pangu-Weather (Huawei)**: Ähnlicher Ansatz
- **GenCast**: Probabilistische Vorhersagen

---

## 8. Robotik und Embodied AI

### Aktuelle Entwicklungen

- **RT-2 (Google)**: Vision-Language-Action Model — Roboter folgen natürlichsprachlichen Anweisungen
- **Figure 01/02**: Humanoider Roboter mit LLM-Integration
- **Tesla Optimus**: Humanoider Roboter, noch in Entwicklung
- **1X NEO**: Humanoider Assistent für den Haushalt

### Paradigmenwechsel

Früher: Roboter werden für jede Aufgabe einzeln programmiert
Jetzt: Foundation Models lernen allgemeine Manipulation und folgen Sprachanweisungen

### Autonomes Fahren

- **Tesla FSD**: Vision-basiert, neuronale Netze für End-to-End-Steuerung
- **Waymo**: Kommerzieller Robotaxi-Dienst (L4 Autonomie)
- **Comma.ai**: Open-Source Fahrassistenz

---

## 9. Grenzen und offene Probleme

### Was KI (noch) nicht zuverlässig kann

| Problem | Beschreibung |
|---------|-------------|
| **Halluzinationen** | LLMs erfinden plausibel klingende aber falsche Fakten |
| **Zuverlässiges Planen** | Langfristige, mehrstufige Pläne über Stunden/Tage hinweg |
| **Kausalverständnis** | Korrelation vs. Kausalität — echtes Verständnis von Ursache-Wirkung fehlt |
| **Selbstbewusstsein** | Kein echtes Verständnis eigener Grenzen und Fehler |
| **Mathematische Garantien** | LLMs machen Rechenfehler, formale Verifikation ist unzuverlässig |
| **Echtzeit-Lernen** | Modelle lernen nicht aus einzelnen Interaktionen (kein Online-Learning) |
| **Common Sense** | Physikalische Intuition und Alltagswissen sind lückenhaft |
| **Lange autonome Arbeit** | Agents verlieren über lange Zeiträume den Faden |

### Aktive Forschungsrichtungen

- **Reasoning-Verbesserung**: Chain-of-Thought, Tree-of-Thought, Process Reward Models
- **Alignment**: Wie stellt man sicher, dass KI menschliche Werte respektiert?
- **Effizienz**: Kleinere Modelle mit gleicher Leistung (Destillation, Quantisierung, MoE)
- **Agentic AI**: Zuverlässigere, sicherere, länger laufende Agents
- **Multimodalität**: Nahtlose Integration aller Sinneskanäle
- **Weltmodelle**: KI, die ein internes Modell der physischen Welt aufbaut

---

## Fazit

Der aktuelle Stand der KI (Frühjahr 2025) ist geprägt von:

1. **LLMs als Fundament** für fast alle KI-Anwendungen
2. **Agents als nächste Evolutionsstufe** — vom Chatbot zum autonomen Handelnden
3. **Multimodalität** als Standard — Text, Bild, Audio, Video verschmelzen
4. **Open Source vs. Closed Source** — gesunder Wettbewerb treibt Innovation
5. **Spezialisierung** — domänenspezifische Modelle für Wissenschaft, Medizin, Recht

Die größte aktuelle Entwicklung ist der Übergang von **passiven Sprachmodellen** zu **aktiven Agent-Systemen**, die selbstständig planen, handeln und iterieren. Genau das findet in diesem Repository statt — Squirrel OS als Haskell-basierte Schnittstelle zu LLM-APIs ist ein Baustein in dieser Entwicklung.
