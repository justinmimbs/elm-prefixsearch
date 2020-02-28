module Filter exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Set exposing (Set)
import TextIndex exposing (TextIndex)


main : Program () Model Msg
main =
    Browser.document
        { init =
            \_ ->
                ( init pl2019
                , Cmd.none
                )
        , update =
            \msg model ->
                ( case msg of
                    EnteredFilter filter ->
                        { model | filter = filter }
                , Cmd.none
                )
        , view =
            \model ->
                Browser.Document
                    "Filter"
                    [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
                    , view model
                    ]
        , subscriptions = \_ -> Sub.none
        }


init : List Item -> Model
init list =
    let
        items =
            list |> List.indexedMap Tuple.pair |> Dict.fromList

        -- build the index
        index =
            Dict.foldl
                (\id item index_ ->
                    (item.title :: item.authors)
                        |> List.concatMap (String.toLower >> String.words)
                        |> List.foldl (TextIndex.insert id) index_
                )
                TextIndex.empty
                items
    in
    { filter = ""
    , items = items
    , index = index
    }


type alias Model =
    { filter : String
    , items : Dict Int Item
    , index : TextIndex
    }


type alias Item =
    { arxiv : String
    , title : String
    , authors : List String
    }


type Msg
    = EnteredFilter String


view : Model -> Html Msg
view { filter, items, index } =
    let
        isDisplayed : Int -> Bool
        isDisplayed =
            case filter |> String.toLower |> String.words of
                [ "" ] ->
                    \_ -> True

                keywords ->
                    let
                        -- search the index
                        matches =
                            index |> TextIndex.searchAll keywords |> Set.fromList
                    in
                    \id -> Set.member id matches
    in
    Html.main_
        []
        [ Html.input
            [ Html.Attributes.type_ "search"
            , Html.Attributes.placeholder "Filter"
            , Html.Attributes.value filter
            , Html.Events.onInput EnteredFilter
            ]
            []
        , Html.ol
            []
            (Dict.foldr
                (\id item nodes ->
                    viewItem (isDisplayed id) item :: nodes
                )
                []
                items
            )
        ]


viewItem : Bool -> Item -> Html a
viewItem isDisplayed { arxiv, title, authors } =
    Html.li
        [ Html.Attributes.style "display"
            (if isDisplayed then
                ""

             else
                "none"
            )
        ]
        [ Html.div []
            [ Html.a
                [ Html.Attributes.href ("https://arxiv.org/abs/" ++ arxiv)
                , Html.Attributes.target "_blank"
                ]
                [ Html.text title
                ]
            ]
        , Html.div
            [ Html.Attributes.class "authors"
            ]
            [ Html.text (authors |> String.join ", ")
            ]
        ]


pl2019 : List Item
pl2019 =
    [ { arxiv = "1901.01111", title = "Information flow in a distributed security setting", authors = [ "Ana Almeida Matos", "Jan Cederquist" ] }
    , { arxiv = "1901.03378", title = "Cocon: Computation in Contextual Type Theory", authors = [ "Brigitte Pientka", "Andreas Abel", "Francisco Ferreira", "David Thibodeau", "Rebecca Zucchini" ] }
    , { arxiv = "1901.03575", title = "Static Analysis for Asynchronous JavaScript Programs", authors = [ "Thodoris Sotiropoulos", "Benjamin Livshits" ] }
    , { arxiv = "1901.03771", title = "Automatic acceleration of Numpy applications on GPUs and multicore CPUs", authors = [ "Mahesh Ravishankar", "Vinod Grover" ] }
    , { arxiv = "1901.04615", title = "AutoPhase: Compiler Phase-Ordering for High Level Synthesis with Deep Reinforcement Learning", authors = [ "Ameer Haj-Ali", "Qijing Huang", "William Moses", "John Xiang", "Ion Stoica", "Krste Asanovic", "John Wawrzynek" ] }
    , { arxiv = "1901.05082", title = "Translation Validation for Security Properties", authors = [ "Matteo Busi", "Pierpaolo Degano", "Letterio Galletta" ] }
    , { arxiv = "1901.05138", title = "Predicting Variable Types in Dynamically Typed Programming Languages", authors = [ "Abhinav Jangda", "Gaurav Anand" ] }
    , { arxiv = "1901.05456", title = "TrABin: Trustworthy Analyses of Binaries", authors = [ "Andreas Lindner", "Roberto Guanciale", "Roberto Metere" ] }
    , { arxiv = "1901.05750", title = "TaDA Live: Compositional Reasoning for Termination of Fine-grained Concurrent Programs", authors = [ "Emanuele D'Osualdo", "Azadeh Farzan", "Philippa Gardner", "Julian Sutherland" ] }
    , { arxiv = "1901.06839", title = "Technical Report: Using Loop Scopes with for-Loops", authors = [ "Nathan Wasser", "Dominic Steinhöfel" ] }
    , { arxiv = "1901.07665", title = "LWeb: Information Flow Security for Multi-tier Web Applications", authors = [ "James Parker", "Niki Vazou", "Michael Hicks" ] }
    , { arxiv = "1901.08006", title = "Safely Abstracting Memory Layouts", authors = [ "Juliana Franco", "Alexandros Tasos", "Sophia Drossopoulou", "Tobias Wrigstad", "Susan Eisenbach" ] }
    , { arxiv = "1901.08840", title = "Program algebra for Turing-machine programs", authors = [ "J. A. Bergstra", "C. A. Middelburg" ] }
    , { arxiv = "1901.08857", title = "Fast, Sound and Effectively Complete Dynamic Race Prediction", authors = [ "Andreas Pavlogiannis" ] }
    , { arxiv = "1901.09056", title = "Not So Fast: Analyzing the Performance of WebAssembly vs. Native Code", authors = [ "Abhinav Jangda", "Bobby Powers", "Emery Berger", "Arjun Guha" ] }
    , { arxiv = "1901.09409", title = "CRAQL: A Composable Language for Querying Source Code", authors = [ "Blake Johnson", "Rahul Simha" ] }
    , { arxiv = "1901.09606", title = "Verifying Asynchronous Interactions via Communicating Session Automata", authors = [ "Julien Lange", "Nobuko Yoshida" ] }
    , { arxiv = "1901.10961", title = "Egyptian multiplication and some of its ramifications", authors = [ "M.H. van Emden" ] }
    , { arxiv = "1901.11100", title = "ExceLint: Automatically Finding Spreadsheet Formula Errors", authors = [ "Daniel W. Barowy", "Emery D. Berger", "Benjamin Zorn" ] }
    , { arxiv = "1901.11411", title = "Eliom: A Language for Modular Tierless Web Programming", authors = [ "Gabriel Radanne", "Jérôme Vouillon", "Vincent Balat" ] }
    , { arxiv = "1901.00073", title = "Proceedings 3rd Workshop on formal reasoning about Causation, Responsibility, and Explanations in Science and Technology", authors = [ "Bernd Finkbeiner", "Samantha Kleinberg" ] }
    , { arxiv = "1901.00428", title = "PrideMM: A Solver for Relaxed Memory Models", authors = [ "Simon Cooksey", "Sarah Harris", "Mark Batty", "Radu Grigore", "Mikoláš Janota" ] }
    , { arxiv = "1901.01001", title = "Identifying Barriers to Adoption for Rust through Online Discourse", authors = [ "Anna Zeng", "Will Crichton" ] }
    , { arxiv = "1901.01930", title = "Keeping CALM: When Distributed Consistency is Easy", authors = [ "Joseph M. Hellerstein", "Peter Alvaro" ] }
    , { arxiv = "1901.02819", title = "Automated Customized Bug-Benchmark Generation", authors = [ "Vineeth Kashyap", "Jason Ruchti", "Lucja Kot", "Emma Turetsky", "Rebecca Swords", "Shih An Pan", "Julien Henry", "David Melski", "Eric Schulte" ] }
    , { arxiv = "1901.03208", title = "PML 2 : Integrated Program Verification in ML", authors = [ "Rodolphe Lepigre" ] }
    , { arxiv = "1901.06087", title = "Modular Verification for Almost-Sure Termination of Probabilistic Programs", authors = [ "Mingzhang Huang", "Hongfei Fu", "Krishnendu Chatterjee", "Amir Kafshdar Goharshady" ] }
    , { arxiv = "1901.06540", title = "Kantorovich Continuity of Probabilistic Programs", authors = [ "Alejandro Aguirre", "Gilles Barthe", "Justin Hsu", "Benjamin Lucien Kaminski", "Joost-Pieter Katoen", "Christoph Matheja" ] }
    , { arxiv = "1901.07820", title = "The Size-Change Principle for Mixed Inductive and Coinductive types", authors = [ "Pierre Hyvernat" ] }
    , { arxiv = "1901.08972", title = "Towards Approximate Mobile Computing", authors = [ "Veljko Pejovic" ] }
    , { arxiv = "1901.10118", title = "ReQWIRE: Reasoning about Reversible Quantum Circuits", authors = [ "Robert Rand", "Jennifer Paykin", "Dong-Ho Lee", "Steve Zdancewic" ] }
    , { arxiv = "1901.10541", title = "Abstract I/O Specification", authors = [ "Willem Penninckx", "Amin Timany", "Bart Jacobs" ] }
    , { arxiv = "1901.10926", title = "Safe Compilation for Hidden Deterministic Hardware Aliasing and Encrypted Computing", authors = [ "Peter T. Breuer" ] }
    , { arxiv = "1901.11054", title = "Noise-Adaptive Compiler Mappings for Noisy Intermediate-Scale Quantum Computers", authors = [ "Prakash Murali", "Jonathan M. Baker", "Ali Javadi Abhari", "Frederic T. Chong", "Margaret Martonosi" ] }
    , { arxiv = "1902.00319", title = "OODIDA: On-board/Off-board Distributed Real-Time Data Analytics for Connected Vehicles", authors = [ "Gregor Ulm", "Simon Smith", "Adrian Nilsson", "Emil Gustavsson", "Mats Jirstrand" ] }
    , { arxiv = "1902.00524", title = "Distributed Reactive Programming for Reactive Distributed Systems", authors = [ "Florian Myter", "Christophe Scholliers", "Wolfgang De Meuter" ] }
    , { arxiv = "1902.00525", title = "ParaSail: A Pointer-Free Pervasively-Parallel Language for Irregular Computations", authors = [ "S. Tucker Taft" ] }
    , { arxiv = "1902.00543", title = "Concrete Syntax with Black Box Parsers", authors = [ "Rodin Aarssen", "Jurgen Vinju", "Tijs van der Storm" ] }
    , { arxiv = "1902.00544", title = "Comprehensive Multiparty Session Types", authors = [ "Andi Bejleri", "Elton Domnori", "Malte Viering", "Patrick Eugster", "Mira Mezini" ] }
    , { arxiv = "1902.00545", title = "Semantic Query Integration With Reason", authors = [ "Philipp Seifer", "Martin Leinberger", "Ralf Lämmel", "Steffen Staab" ] }
    , { arxiv = "1902.00546", title = "Separating Use and Reuse to Improve Both", authors = [ "Hrshikesh Arora", "Marco Servetto", "Bruno C. D. S. Oliveira" ] }
    , { arxiv = "1902.00548", title = "Shallow EDSLs and Object-Oriented Programming: Beyond Simple Compositionality", authors = [ "Weixin Zhang", "Bruno Oliveira" ] }
    , { arxiv = "1902.00735", title = "Event Loops as First-Class Values: A Case Study in Pedagogic Language Design", authors = [ "Joe Politz", "Benjamin Lerner", "Sorawee Porncharoenwase", "Shriram Krishnamurthi" ] }
    , { arxiv = "1902.01906", title = "Fearless Concurrency? Understanding Concurrent Programming Safety in Real-World Rust Software", authors = [ "Zeming Yu", "Linhai Song", "Yiying Zhang" ] }
    , { arxiv = "1902.02816", title = "Revec: Program Rejuvenation through Revectorization", authors = [ "Charith Mendis", "Ajay Jain", "Paras Jain", "Saman Amarasinghe" ] }
    , { arxiv = "1902.04645", title = "Program Equivalence for Algebraic Effects via Modalities", authors = [ "Cristina Matache" ] }
    , { arxiv = "1902.04659", title = "Cost Analysis of Nondeterministic Probabilistic Programs", authors = [ "Peixin Wang", "Hongfei Fu", "Amir Kafshdar Goharshady", "Krishnendu Chatterjee", "Xudong Qin", "Wenjun Shi" ] }
    , { arxiv = "1902.04738", title = "Mesh: Compacting Memory Management for C/C++ Applications", authors = [ "Bobby Powers", "David Tench", "Emery D. Berger", "Andrew McGregor" ] }
    , { arxiv = "1902.04744", title = "Proving Expected Sensitivity of Probabilistic Programs with Randomized Variable-Dependent Termination Time", authors = [ "Peixin Wang", "Hongfei Fu", "Krishnendu Chatterjee", "Yuxin Deng", "Ming Xu" ] }
    , { arxiv = "1902.05178", title = "Spectre is here to stay: An analysis of side-channels and speculative execution", authors = [ "Ross Mcilroy", "Jaroslav Sevcik", "Tobias Tebbi", "Ben L. Titzer", "Toon Verwaest" ] }
    , { arxiv = "1902.05205", title = "HyPLC: Hybrid Programmable Logic Controller Program Translation for Verification", authors = [ "Luis Garcia", "Stefan Mitsch", "Andre Platzer" ] }
    , { arxiv = "1902.05283", title = "Reasoning About a Machine with Local Capabilities: Provably Safe Stack and Return Pointer Management - Technical Appendix Including Proofs and Details", authors = [ "Lau Skorstengaard", "Dominique Devriese", "Lars Birkedal" ] }
    , { arxiv = "1902.05369", title = "Introducing Yet Another REversible Language", authors = [ "Claudio Grandi", "Dariush Moshiri", "Luca Roversi" ] }
    , { arxiv = "1902.05464", title = "We should Stop Claiming Generality in our Domain-Specific Language Papers", authors = [ "Daco Harkes" ] }
    , { arxiv = "1902.05594", title = "Variability Abstraction and Refinement for Game-based Lifted Model Checking of full CTL (Extended Version)", authors = [ "Aleksandar S. Dimovski", "Axel Legay", "Andrzej Wasowski" ] }
    , { arxiv = "1902.05870", title = "Formal Foundations of Serverless Computing", authors = [ "Abhinav Jangda", "Donald Pinckney", "Yuriy Brun", "Arjun Guha" ] }
    , { arxiv = "1902.06056", title = "Resource-Aware Session Types for Digital Contracts", authors = [ "Ankush Das", "Stephanie Balzer", "Jan Hoffmann", "Frank Pfenning", "Ishani Santurkar" ] }
    , { arxiv = "1902.06097", title = "Normalization by Evaluation for Call-by-Push-Value and Polarized Lambda-Calculus", authors = [ "Andreas Abel", "Christian Sattler" ] }
    , { arxiv = "1902.06590", title = "A Dependently Typed Library for Static Information-Flow Control in Idris", authors = [ "Simon Gregersen", "Søren Eller Thomsen", "Aslan Askarov" ] }
    , { arxiv = "1902.06950", title = "Composing bidirectional programs monadically (with appendices)", authors = [ "Li-yao Xia", "Dominic Orchard", "Meng Wang" ] }
    , { arxiv = "1902.07808", title = "Optimizing and Evaluating Transient Gradual Typing", authors = [ "Michael M. Vitousek", "Jeremy G. Siek", "Avik Chaudhuri" ] }
    , { arxiv = "1902.07818", title = "Proceedings Fifth International Workshop on Rewriting Techniques for Program Transformations and Evaluation", authors = [ "Joachim Niehren", "David Sabel" ] }
    , { arxiv = "1902.08422", title = "Optimizing Space of Parallel Processes", authors = [ "Manfred Schmidt-Schauß", "Nils Dallmeyer" ] }
    , { arxiv = "1902.08726", title = "A Hybrid Formal Verification System in Coq for Ensuring the Reliability and Security of Ethereum-based Service Smart Contracts", authors = [ "Zheng Yang", "Hang Lei", "Weizhong Qian" ] }
    , { arxiv = "1902.09502", title = "Reliable State Machines: A Framework for Programming Reliable Cloud Services", authors = [ "Suvam Mukherjee", "Nitin John Raj", "Krishnan Govindraj", "Pantazis Deligiannis", "Chandramouleswaran Ravichandran", "Akash Lal", "Aseem Rastogi", "Raja Krishnaswamy" ] }
    , { arxiv = "1902.09685", title = "Iteratively Composing Statically Verified Traits", authors = [ "Isaac Oscar Gariano", "Marco Servetto", "Alex Potanin", "Hrshikesh Arora" ] }
    , { arxiv = "1902.09927", title = "The Cpi-calculus: a Model for Confidential Name Passing", authors = [ "Ivan Prokić" ] }
    , { arxiv = "1902.10231", title = "Sound Invariant Checking Using Type Modifiers and Object Capabilities", authors = [ "Isaac Oscar Gariano", "Marco Servetto", "Alex Potanin" ] }
    , { arxiv = "1902.10345", title = "Stateful Dataflow Multigraphs: A Data-Centric Model for Performance Portability on Heterogeneous Architectures", authors = [ "Tal Ben-Nun", "Johannes de Fine Licht", "Alexandros Nikolaos Ziogas", "Timo Schneider", "Torsten Hoefler" ] }
    , { arxiv = "1902.00660", title = "Fine-Grain Checkpointing with In-Cache-Line Logging", authors = [ "Nachshon Cohen", "David T. Aksun", "Hillel Avni", "James R. Larus" ] }
    , { arxiv = "1902.01510", title = "Proceedings Tenth International Workshop on Computing with Terms and Graphs", authors = [ "Maribel Fernández", "Ian Mackie" ] }
    , { arxiv = "1902.02012", title = "On Quasi Ordinal Diagram Systems", authors = [ "Mitsuhiro Okada", "Yuta Takahashi" ] }
    , { arxiv = "1902.04373", title = "Polynomial Invariant Generation for Non-deterministic Recursive Programs", authors = [ "Krishnendu Chatterjee", "Hongfei Fu", "Amir Kafshdar Goharshady", "Ehsan Kafshdar Goharshady" ] }
    , { arxiv = "1902.04836", title = "Differentials and distances in probabilistic coherence spaces", authors = [ "Thomas Ehrhard" ] }
    , { arxiv = "1902.05436", title = "Checking Observational Purity of Procedures", authors = [ "Himanshu Arora", "Raghavan Komondoor", "G. Ramalingam" ] }
    , { arxiv = "1902.05462", title = "Redundant Loads: A Software Inefficiency Indicator", authors = [ "Pengfei Su", "Shasha Wen", "Hailong Yang", "Milind Chabbi", "Xu Liu" ] }
    , { arxiv = "1902.05945", title = "Types by Need (Extended Version)", authors = [ "Beniamino Accattoli", "Giulio Guerrieri", "Maico Leberle" ] }
    , { arxiv = "1902.05983", title = "Robustness of Neural Networks: A Probabilistic and Practical Approach", authors = [ "Ravi Mangal", "Aditya V. Nori", "Alessandro Orso" ] }
    , { arxiv = "1902.06146", title = "Compiled Obfuscation for Data Structures in Encrypted Computing", authors = [ "Peter T. Breuer" ] }
    , { arxiv = "1902.06733", title = "A static higher-order dependency pair framework", authors = [ "Carsten Fuhs", "Cynthia Kop" ] }
    , { arxiv = "1902.07230", title = "Uniform Substitution At One Fell Swoop", authors = [ "André Platzer" ] }
    , { arxiv = "1902.07515", title = "The Weak Call-By-Value λ-Calculus is Reasonable for Both Time and Space", authors = [ "Yannick Forster", "Fabian Kunze", "Marc Roth" ] }
    , { arxiv = "1902.07986", title = "Probabilistic Smart Contracts: Secure Randomness on the Blockchain", authors = [ "Krishnendu Chatterjee", "Amir Kafshdar Goharshady", "Arash Pourdamghani" ] }
    , { arxiv = "1902.08419", title = "Reducing Total Correctness to Partial Correctness by a Transformation of the Language Semantics", authors = [ "Andrei-Sebastian Buruiană", "Ştefan Ciobâcă" ] }
    , { arxiv = "1902.08420", title = "Automating the Diagram Method to Prove Correctness of Program Transformations", authors = [ "David Sabel" ] }
    , { arxiv = "1902.08421", title = "On Transforming Functions Accessing Global Variables into Logically Constrained Term Rewriting Systems", authors = [ "Yoshiaki Kanazawa", "Naoki Nishida" ] }
    , { arxiv = "1902.09099", title = "Mitigating Power Side Channels during Compilation", authors = [ "Jingbo Wang", "Chungha Sung", "Chao Wang" ] }
    , { arxiv = "1902.09122", title = "Neural Reverse Engineering of Stripped Binaries", authors = [ "Yaniv David", "Uri Alon", "Eran Yahav" ] }
    , { arxiv = "1902.09334", title = "A Systematic Impact Study for Fuzzer-Found Compiler Bugs", authors = [ "Michaël Marcozzi", "Qiyi Tang", "Alastair F. Donaldson", "Cristian Cadar" ] }
    , { arxiv = "1902.11189", title = "Semantics of higher-order probabilistic programs with conditioning", authors = [ "Fredrik Dahlqvist", "Dexter Kozen" ] }
    , { arxiv = "1902.08091", title = "On the qubit routing problem", authors = [ "Alexander Cowtan", "Silas Dilkes", "Ross Duncan", "Alexandre Krajenbrink", "Will Simmons", "Seyon Sivarajah" ] }
    , { arxiv = "1903.00982", title = "Oxide: The Essence of Rust", authors = [ "Aaron Weiss", "Daniel Patterson", "Nicholas D. Matsakis", "Amal Ahmed" ] }
    , { arxiv = "1903.01237", title = "Dijkstra Monads for All", authors = [ "Kenji Maillard", "Danel Ahman", "Robert Atkey", "Guido Martinez", "Catalin Hritcu", "Exequiel Rivas", "Éric Tanter" ] }
    , { arxiv = "1903.01665", title = "Custom Code Generation for a Graph DSL", authors = [ "Bikash Gogoi", "Unnikrishnan Cheramangalath", "Rupesh Nasre" ] }
    , { arxiv = "1903.01855", title = "TensorFlow Eager: A Multi-Stage, Python-Embedded DSL for Machine Learning", authors = [ "Akshay Agrawal", "Akshay Naresh Modi", "Alexandre Passos", "Allen Lavoie", "Ashish Agarwal", "Asim Shankar", "Igor Ganichev", "Josh Levenberg", "Mingsheng Hong", "Rajat Monga", "Shanqing Cai" ] }
    , { arxiv = "1903.03229", title = "Deductive Optimization of Relational Data Storage", authors = [ "John K. Feser", "Samuel Madden", "Nan Tang", "Armando Solar-Lezama" ] }
    , { arxiv = "1903.03276", title = "Formal Constraint-based Compilation for Noisy Intermediate-Scale Quantum Systems", authors = [ "Prakash Murali", "Ali Javadi-Abhari", "Frederic T. Chong", "Margaret Martonosi" ] }
    , { arxiv = "1903.05333", title = "SymPas: Symbolic Program Slicing", authors = [ "Yingzhou Zhang" ] }
    , { arxiv = "1903.05879", title = "Simply RaTT: A Fitch-style Modal Calculus for Reactive Programming without Space Leaks", authors = [ "Patrick Bahr", "Christian Graulund", "Rasmus Møgelberg" ] }
    , { arxiv = "1903.06119", title = "Correct Approximation of IEEE 754 Floating-Point Arithmetic for Program Verification", authors = [ "Roberto Bagnara", "Abramo Bagnara", "Fabio Biselli", "Michele Chiari", "Roberta Gori" ] }
    , { arxiv = "1903.06407", title = "Get rid of inline assembly through verification-oriented lifting", authors = [ "Frédéric Recoules", "Sébastien Bardin", "Richard Bonichon", "Laurent Mounier", "Marie-Laure Potet" ] }
    , { arxiv = "1903.06560", title = "Replication-Aware Linearizability", authors = [ "Constantin Enea", "Suha Orhun Mutluergil", "Gustavo Petri", "Chao Wang" ] }
    , { arxiv = "1903.07038", title = "Compiler-assisted Adaptive Program Scheduling in big.LITTLE Systems", authors = [ "Marcelo Novaes", "Vinícius Petrucci", "Abdoulaye Gamatié", "Fernando Quintão" ] }
    , { arxiv = "1903.07213", title = "Specification and Inference of Trace Refinement Relations", authors = [ "Timos Antonopoulos", "Eric Koskinen", "Ton-Chanh Le" ] }
    , { arxiv = "1903.07962", title = "No more, no less - A formal model for serverless computing", authors = [ "Maurizio Gabbrielli", "Saverio Giallorenzo", "Ivan Lanese", "Fabrizio Montesi", "Marco Peressotti", "Stefano Pio Zingaro" ] }
    , { arxiv = "1903.08233", title = "Elaborating Inductive Definitions and Course-of-Values Induction in Cedille", authors = [ "Christopher Jenkins", "Colin McDonald", "Aaron Stump" ] }
    , { arxiv = "1903.09713", title = "SLING: Using Dynamic Analysis to Infer Program Invariants in Separation Logic", authors = [ "Ton Chanh Le", "Guolong Zheng", "ThanhVu Nguyen" ] }
    , { arxiv = "1903.10267", title = "On Evaluating the Renaissance Benchmarking Suite: Variety, Performance, and Complexity", authors = [ "Aleksandar Prokopec", "Andrea Rosà", "David Leopoldseder", "Gilles Duboscq", "Petr Tůma", "Martin Studener", "Lubomír Bulej", "Yudi Zheng", "Alex Villazón", "Doug Simon", "Thomas Wuerthinger", "Walter Binder" ] }
    , { arxiv = "1903.10556", title = "The Random Conditional Distribution for Higher-Order Probabilistic Inference", authors = [ "Zenna Tavares", "Xin Zhang", "Edgar Minaysan", "Javier Burroni", "Rajesh Ranganath", "Armando Solar Lezama" ] }
    , { arxiv = "1903.10677", title = "Generalized Convolution and Efficient Language Recognition", authors = [ "Conal Elliott" ] }
    , { arxiv = "1903.11397", title = "Lost in translation: Exposing hidden compiler optimization opportunities", authors = [ "Kyriakos Georgiou", "Zbigniew Chamski", "Andres Amaya Garcia", "David May", "Kerstin Eder" ] }
    , { arxiv = "1903.11765", title = "Connecting Program Synthesis and Reachability: Automatic Program Repair using Test-Input Generation", authors = [ "ThanhVu Nguyen", "Westley Weimer", "Deepak Kapur", "Stephanie Forrest" ] }
    , { arxiv = "1903.12254", title = "Proving Differential Privacy with Shadow Execution", authors = [ "Yuxin Wang", "Zeyu Ding", "Guanhong Wang", "Daniel Kifer", "Danfeng Zhang" ] }
    , { arxiv = "1903.02482", title = "LF-PPL: A Low-Level First Order Probabilistic Programming Language for Non-Differentiable Models", authors = [ "Yuan Zhou", "Bradley J. Gram-Hansen", "Tobias Kohn", "Tom Rainforth", "Hongseok Yang", "Frank Wood" ] }
    , { arxiv = "1903.02835", title = "Only Connect, Securely", authors = [ "Chandrika Bhardwaj", "Sanjiva Prasad" ] }
    , { arxiv = "1903.03501", title = "Certifying Safety when Implementing Consensus", authors = [ "Aurojit Panda" ] }
    , { arxiv = "1903.03671", title = "Lenses and Learners", authors = [ "Brendan Fong", "Michael Johnson" ] }
    , { arxiv = "1903.05126", title = "Induction, Coinduction, and Fixed Points in PL Type Theory", authors = [ "Moez A. AbdelGawad" ] }
    , { arxiv = "1903.06514", title = "Mutual Coinduction", authors = [ "Moez A. AbdelGawad" ] }
    , { arxiv = "1903.09477", title = "Facilitating Rapid Prototyping in the OODIDA Data Analytics Platform via Active-Code Replacement", authors = [ "Gregor Ulm", "Simon Smith", "Adrian Nilsson", "Emil Gustavsson", "Mats Jirstrand" ] }
    , { arxiv = "1903.12366", title = "Using Structured Input and Modularity for Improved Learning", authors = [ "Zehra Sura", "Tong Chen", "Hyojin Sung" ] }
    , { arxiv = "1903.12519", title = "A Provable Defense for Deep Residual Networks", authors = [ "Matthew Mirman", "Gagandeep Singh", "Martin Vechev" ] }
    , { arxiv = "1904.00396", title = "Proceedings Programming Language Approaches to Concurrency- and Communication-cEntric Software", authors = [ "Francisco Martins", "Dominic Orchard" ] }
    , { arxiv = "1904.01009", title = "A benchmark for C program verification", authors = [ "Marko van Eekelen", "Daniil Frumin", "Herman Geuvers", "Léon Gondelman", "Robbert Krebbers", "Marc Schoolderman", "Sjaak Smetsers", "Freek Verbeek", "Benoît Viguier", "Freek Wiedijk" ] }
    , { arxiv = "1904.01031", title = "Modular Synthesis of Divide-and-Conquer Parallelism for Nested Loops (Extended Version)", authors = [ "Azadeh Farzan", "Victor Nicolet" ] }
    , { arxiv = "1904.01283", title = "Service Equivalence via Multiparty Session Type Isomorphisms", authors = [ "Assel Altayeva", "Nobuko Yoshida" ] }
    , { arxiv = "1904.01284", title = "FreeST: Context-free Session Types in a Functional Language", authors = [ "Bernardo Almeida", "Andreia Mordido", "Vasco T. Vasconcelos" ] }
    , { arxiv = "1904.01286", title = "Concurrent Typestate-Oriented Programming in Java", authors = [ "Rosita Gerbo", "Luca Padovani" ] }
    , { arxiv = "1904.01287", title = "Multiparty Session Type-safe Web Development with Static Linearity", authors = [ "Jonathan King", "Nicholas Ng", "Nobuko Yoshida" ] }
    , { arxiv = "1904.01288", title = "Value-Dependent Session Design in a Dependently Typed Language", authors = [ "Jan de Muijnck-Hughes", "Edwin Brady", "Wim Vanderbauwhede" ] }
    , { arxiv = "1904.01290", title = "A Message-Passing Interpretation of Adjoint Logic", authors = [ "Klaas Pruiksma", "Frank Pfenning" ] }
    , { arxiv = "1904.02079", title = "Symbolic Exact Inference for Discrete Probabilistic Programs", authors = [ "Steven Holtzen", "Todd Millstein", "Guy Van den Broeck" ] }
    , { arxiv = "1904.02809", title = "Proving tree algorithms for succinct data structures", authors = [ "Reynald Affeldt", "Jacques Garrigue", "Xuanrui Qi", "Kazunari Tanaka" ] }
    , { arxiv = "1904.03343", title = "Exploring and Benchmarking High Performance & Scientific Computing using R R HPC Packages and Lower level compiled languages A Comparative Study", authors = [ "Rahim K. Charania" ] }
    , { arxiv = "1904.03383", title = "On the Representation of Partially Specified Implementations and its Application to the Optimization of Linear Algebra Kernels on GPU", authors = [ "Ulysse Beaugnon", "Basile Clément", "Nicolas Tollenaere", "Albert Cohen" ] }
    , { arxiv = "1904.03521", title = "Type-Level Computations for Ruby Libraries", authors = [ "Milod Kazerounian", "Sankha Narayan Guria", "Niki Vazou", "Jeffrey S. Foster", "David Van Horn" ] }
    , { arxiv = "1904.03540", title = "Mek: Mechanics Prototyping Tool for 2D Tile-Based Turn-Based Deterministic Games", authors = [ "Rokas Volkovas", "Michael Fairbank", "John Woodward", "Simon Lucas" ] }
    , { arxiv = "1904.04371", title = "A HoTT Quantum Equational Theory (Extended Version)", authors = [ "Jennifer Paykin", "Steve Zdancewic" ] }
    , { arxiv = "1904.05387", title = "Tea: A High-level Language and Runtime System for Automating Statistical Analysis", authors = [ "Eunice Jun", "Maureen Daum", "Jared Roesch", "Sarah E. Chasins", "Emery D. Berger", "Rene Just", "Katharina Reinecke" ] }
    , { arxiv = "1904.05389", title = "Compiling a Calculus for Relaxed Memory: Practical constraint-based low-level concurrency", authors = [ "Michael J. Sullivan", "Karl Crary", "Salil Joshi" ] }
    , { arxiv = "1904.05498", title = "Synthesizing Database Programs for Schema Refactoring", authors = [ "Yuepeng Wang", "James Dong", "Rushi Shah", "Isil Dillig" ] }
    , { arxiv = "1904.05980", title = "Parallel algorithms development for programmable logic devices", authors = [ "Issam Damaj" ] }
    , { arxiv = "1904.06534", title = "Flint for Safer Smart Contracts", authors = [ "Franklin Schrans", "Daniel Hails", "Alexander Harkness", "Sophia Drossopoulou", "Susan Eisenbach" ] }
    , { arxiv = "1904.06584", title = "Got: Git, but for Objects", authors = [ "Rohan Achar", "Cristina V. Lopes" ] }
    , { arxiv = "1904.06750", title = "From Theory to Systems: A Grounded Approach to Programming Language Education", authors = [ "Will Crichton" ] }
    , { arxiv = "1904.07061", title = "Sound, Fine-Grained Traversal Fusion for Heterogeneous Trees - Extended Version", authors = [ "Laith Sakka", "Kirshanthan Sundararajah", "Ryan R. Newton", "Milind Kulkarni" ] }
    , { arxiv = "1904.07136", title = "Specifying Concurrent Programs in Separation Logic: Morphisms and Simulations", authors = [ "Aleksandar Nanevski", "Anindya Banerjee", "Germán Andrés Delbianco", "Ignacio Fábregas" ] }
    , { arxiv = "1904.07146", title = "SyGuS-Comp 2018: Results and Analysis", authors = [ "Rajeev Alur", "Dana Fisman", "Saswat Padhi", "Rishabh Singh", "Abhishek Udupa" ] }
    , { arxiv = "1904.07298", title = "A Path To DOT: Formalizing Fully Path-Dependent Types", authors = [ "Marianna Rapoport", "Ondřej Lhoták" ] }
    , { arxiv = "1904.07415", title = "Resource-Guided Program Synthesis", authors = [ "Tristan Knoth", "Di Wang", "Nadia Polikarpova", "Jan Hoffmann" ] }
    , { arxiv = "1904.07425", title = "The Geometry of Bayesian Programming", authors = [ "Ugo Dal Lago", "Naohiko Hoshino" ] }
    , { arxiv = "1904.08096", title = "Scalable Verification of Probabilistic Networks", authors = [ "Steffen Smolka", "Praveen Kumar", "David M Kahn", "Nate Foster", "Justin Hsu", "Dexter Kozen", "Alexandra Silva" ] }
    , { arxiv = "1904.08555", title = "ClangJIT: Enhancing C++ with Just-in-Time Compilation", authors = [ "Hal Finkel", "David Poliakoff", "David F. Richards" ] }
    , { arxiv = "1904.08722", title = "Quantitative Expressiveness of Instruction Sequence Classes for Computation on Single Bit Registers", authors = [ "Jan A. Bergstra" ] }
    , { arxiv = "1904.09959", title = "Optimization and Abstraction: A Synergistic Approach for Analyzing Neural Network Robustness", authors = [ "Greg Anderson", "Shankara Pailoor", "Isil Dillig", "Swarat Chaudhuri" ] }
    , { arxiv = "1904.10107", title = "A Syntactic Model of Mutation and Aliasing", authors = [ "Paola Giannini", "Marco Servetto", "Elena Zucca" ] }
    , { arxiv = "1904.11170", title = "Abstract Interpretation under Speculative Execution", authors = [ "Meng Wu", "Chao Wang" ] }
    , { arxiv = "1904.11254", title = "SafeStrings: Representing Strings as Structured Data", authors = [ "David Kelly", "Mark Marron", "David Clark", "Earl T. Barr" ] }
    , { arxiv = "1904.11327", title = "Ephemeral Data Handling in Microservices - Technical Report", authors = [ "Saverio Giallorenzo", "Fabrizio Montesi", "Larisa Safina", "Stefano Pio Zingaro" ] }
    , { arxiv = "1904.13049", title = "Targeted Synthesis for Programming with Data Invariants", authors = [ "John Sarracino", "Shraddha Barke", "Hila Peleg", "Sorin Lerner", "Nadia Polikarpova" ] }
    , { arxiv = "1904.13088", title = "Dependence-Aware, Unbounded Sound Predictive Race Detection", authors = [ "Kaan Genç", "Jake Roemer", "Yufan Xu", "Michael D. Bond" ] }
    , { arxiv = "1904.01117", title = "Aiming Low Is Harder -- Induction for Lower Bounds in Probabilistic Program Verification", authors = [ "Marcel Hark", "Benjamin Lucien Kaminski", "Jürgen Giesl", "Joost-Pieter Katoen" ] }
    , { arxiv = "1904.02830", title = "An Evolutionary Framework for Automatic and Guided Discovery of Algorithms", authors = [ "Ruchira Sasanka", "Konstantinos Krommydas" ] }
    , { arxiv = "1904.03061", title = "A Literature Study of Embeddings on Source Code", authors = [ "Zimin Chen", "Martin Monperrus" ] }
    , { arxiv = "1904.04304", title = "Verification Logics for Quantum Programs", authors = [ "Robert Rand" ] }
    , { arxiv = "1904.06159", title = "Proceedings Joint International Workshop on Linearity & Trends in Linear Logic and Applications", authors = [ "Thomas Ehrhard", "Maribel Fernández", "Valeria de Paiva", "Lorenzo Tortora de Falco" ] }
    , { arxiv = "1904.06319", title = "Verified Optimization in a Quantum Intermediate Representation", authors = [ "Kesha Hietala", "Robert Rand", "Shih-Han Hung", "Xiaodi Wu", "Michael Hicks" ] }
    , { arxiv = "1904.06845", title = "The Bang Calculus and the Two Girard's Translations", authors = [ "Giulio Guerrieri", "Giulio Manzonetto" ] }
    , { arxiv = "1904.06846", title = "From Linear Logic to Cyclic Sharing", authors = [ "Masahito Hasegawa" ] }
    , { arxiv = "1904.06847", title = "On the Lambek Calculus with an Exchange Modality", authors = [ "Jiaming Jiang", "Harley Eades III", "Valeria de Paiva" ] }
    , { arxiv = "1904.06848", title = "Taking Linear Logic Apart", authors = [ "Wen Kokke", "Fabrizio Montesi", "Marco Peressotti" ] }
    , { arxiv = "1904.07404", title = "swTVM: Exploring the Automated Compilation for Deep Learning on Sunway Architecture", authors = [ "Changxi Liu", "Hailong Yang", "Rujun Sun", "Zhongzhi Luan", "Lin Gan", "Guangwen Yang", "Depei Qian" ] }
    , { arxiv = "1904.07463", title = "Using Dynamic Analysis to Generate Disjunctive Invariants", authors = [ "ThanhVu Nguyen", "Deepak Kapur", "Westley Weimer", "Stephanie Forrest" ] }
    , { arxiv = "1904.08368", title = "Relay: A High-Level Compiler for Deep Learning", authors = [ "Jared Roesch", "Steven Lyubomirsky", "Marisa Kirisame", "Logan Weber", "Josh Pollock", "Luis Vega", "Ziheng Jiang", "Tianqi Chen", "Thierry Moreau", "Zachary Tatlock" ] }
    , { arxiv = "1904.08380", title = "Low-Latency Graph Streaming Using Compressed Purely-Functional Trees", authors = [ "Laxman Dhulipala", "Julian Shun", "Guy Blelloch" ] }
    , { arxiv = "1904.09429", title = "Chaotic Compilation for Encrypted Computing: Obfuscation but Not in Name", authors = [ "Peter T. Breuer" ] }
    , { arxiv = "1904.09561", title = "Proceedings Twelfth Workshop on Developments in Computational Models and Ninth Workshop on Intersection Types and Related Systems", authors = [ "Michele Pagani", "Sandra Alves" ] }
    , { arxiv = "1904.09818", title = "One DSL to Rule Them All: IDE-Assisted Code Generation for Agile Data Analysis", authors = [ "Artur Andrzejak", "Oliver Wenz", "Diego Costa" ] }
    , { arxiv = "1904.09946", title = "Strand Spaces with Choice via a Process Algebra Semantics", authors = [ "Fan Yang", "Santiago Escobar", "Catherine Meadows", "José Meseguer" ] }
    , { arxiv = "1904.10800", title = "Towards a Semantic Measure of the Execution Time in Call-by-Value lambda-Calculus", authors = [ "Giulio Guerrieri" ] }
    , { arxiv = "1904.11818", title = "A certifying extraction with time bounds from Coq to call-by-value λ-calculus", authors = [ "Yannick Forster", "Fabian Kunze" ] }
    , { arxiv = "1904.11968", title = "Learning Semantic Vector Representations of Source Code via a Siamese Neural Network", authors = [ "David Wehr", "Halley Fede", "Eleanor Pence", "Bo Zhang", "Guilherme Ferreira", "John Walczyk", "Joseph Hughes" ] }
    , { arxiv = "1904.12137", title = "Differential Logical Relations, Part I: The Simply-Typed Case (Long Version)", authors = [ "Ugo Dal Lago", "Francesco Gavazzo", "Akira Yoshimizu" ] }
    , { arxiv = "1904.12210", title = "A Practical Analysis of Rust's Concurrency Story", authors = [ "Aditya Saligrama", "Andrew Shen", "Jon Gjengset" ] }
    , { arxiv = "1904.12501", title = "A Framework for Debugging Java Programs in a Bytecode", authors = [ "Safeeullah Soomro", "Mohammad Riyaz Belgaum", "Zainab Alansari", "Mahdi H Miraz" ] }
    , { arxiv = "1904.13338", title = "Behavioral Program Logic and LAGC Semantics without Continuations (Technical Report)", authors = [ "Eduard Kamburjan" ] }
    , { arxiv = "1904.08083", title = "A 2-Categorical Study of Graded and Indexed Monads", authors = [ "Soichiro Fujii" ] }
    , { arxiv = "1904.09041", title = "Optimizing Quantum Programs against Decoherence: Delaying Qubits into Quantum Superposition", authors = [ "Yu Zhang", "Haowei Deng", "Quanxi Li", "Haoze Song", "Leihai Nie" ] }
    , { arxiv = "1905.00402", title = "Next-Paradigm Programming Languages: What Will They Look Like and What Changes Will They Bring?", authors = [ "Yannis Smaragdakis" ] }
    , { arxiv = "1905.01453", title = "A Type System for First-Class Layers with Inheritance, Subtyping, and Swapping", authors = [ "Hiroaki Inoue", "Atsushi Igarashi" ] }
    , { arxiv = "1905.01469", title = "An experiment with denotational semantics", authors = [ "Blikle Andrzej" ] }
    , { arxiv = "1905.01473", title = "A Denotational Engineering of Programming Languages", authors = [ "Blikle Andrzej" ] }
    , { arxiv = "1905.02033", title = "TryLinks: An interactive tutorial system for a cross-tier Web programming language", authors = [ "Junao Wu", "Arek Mikolajczak", "James Cheney" ] }
    , { arxiv = "1905.02051", title = "Language-integrated provenance by trace analysis", authors = [ "Stefan Fehrenbach", "James Cheney" ] }
    , { arxiv = "1905.02088", title = "Heaps Don't Lie: Countering Unsoundness with Heap Snapshots", authors = [ "Neville Grech", "George Fourtounis", "Adrian Francalanza", "Yannis Smaragdakis" ] }
    , { arxiv = "1905.02145", title = "Automatic Syntax Error Reporting and Recovery in Parsing Expression Grammars", authors = [ "Sérgio Queiroz de Medeiros", "Gilney de Azevedo Alvez Junior", "Fabio Mascarenhas" ] }
    , { arxiv = "1905.02529", title = "Programming Unikernels in the Large via Functor Driven Development", authors = [ "Gabriel Radanne", "Thomas Gazagnaire", "Anil Madhavapeddy", "Jeremy Yallop", "Richard Mortier", "Hannes Mehnert", "Mindy Preston", "David Scott" ] }
    , { arxiv = "1905.03746", title = "Research Note: An Open Source Bluespec Compiler", authors = [ "David J. Greaves" ] }
    , { arxiv = "1905.03957", title = "Dynamic Verification with Observational Equivalence of C/C++ Concurrency", authors = [ "Sanjana Singh", "Divyanjali Sharma", "Subodh Sharma" ] }
    , { arxiv = "1905.04366", title = "A true concurrent model of smart contracts executions", authors = [ "Massimo Bartoletti", "Letterio Galletta", "Maurizio Murgia" ] }
    , { arxiv = "1905.05251", title = "Learning Scalable and Precise Representation of Program Semantics", authors = [ "Ke Wang" ] }
    , { arxiv = "1905.05684", title = "Automated Parameterized Verification of CRDTs", authors = [ "Kartik Nagar", "Suresh Jagannathan" ] }
    , { arxiv = "1905.05800", title = "Proving Unrealizability for Syntax-Guided Synthesis", authors = [ "Qinheping Hu", "Jason Breck", "John Cyphert", "Loris D'Antoni", "Thomas Reps" ] }
    , { arxiv = "1905.05909", title = "Proceedings ML Family / OCaml Users and Developers workshops", authors = [ "Sam Lindley", "Gabriel Scherer" ] }
    , { arxiv = "1905.06495", title = "Loop Summarization with Rational Vector Addition Systems (extended version)", authors = [ "Jake Silverman", "Zachary Kincaid" ] }
    , { arxiv = "1905.06543", title = "Extending OCaml's 'open'", authors = [ "Runhang Li", "Jeremy Yallop" ] }
    , { arxiv = "1905.06544", title = "Effects Without Monads: Non-determinism -- Back to the Meta Language", authors = [ "Oleg Kiselyov" ] }
    , { arxiv = "1905.06545", title = "Direct Interpretation of Functional Programs for Debugging", authors = [ "John Whitington", "Tom Ridge" ] }
    , { arxiv = "1905.06546", title = "First-Class Subtypes", authors = [ "Jeremy Yallop", "Stephen Dolan" ] }
    , { arxiv = "1905.07212", title = "Implementing a Library for Probabilistic Programming using Non-strict Non-determinism", authors = [ "Sandra Dylus", "Jan Christiansen", "Finn Teegen" ] }
    , { arxiv = "1905.07362", title = "Simple and Effective Relation-Based Approaches To XPath and XSLT Type Checking (Technical Report, Bad Honnef 2015)", authors = [ "Baltasar Trancón y Widemann", "Markus Lepper" ] }
    , { arxiv = "1905.07457", title = "Overfitting in Synthesis: Theory and Practice (Extended Version)", authors = [ "Saswat Padhi", "Todd Millstein", "Aditya Nori", "Rahul Sharma" ] }
    , { arxiv = "1905.07639", title = "Developing secure Bitcoin contracts with BitML", authors = [ "Nicola Atzei", "Massimo Bartoletti", "Stefano Lande", "Nobuko Yoshida", "Roberto Zunino" ] }
    , { arxiv = "1905.07705", title = "Property Directed Self Composition", authors = [ "Ron Shemer", "Arie Gurfinkel", "Sharon Shoham", "Yakir Vizel" ] }
    , { arxiv = "1905.07739", title = "Inferring Inductive Invariants from Phase Structures", authors = [ "Yotam M. Y. Feldman", "James R. Wilcox", "Sharon Shoham", "Mooly Sagiv" ] }
    , { arxiv = "1905.07805", title = "Verification of Threshold-Based Distributed Algorithms by Decomposition to Decidable Logics", authors = [ "Idan Berkovits", "Marijana Lazic", "Giuliano Losa", "Oded Padon", "Sharon Shoham" ] }
    , { arxiv = "1905.08178", title = "Partial Redundancy Elimination using Lazy Code Motion", authors = [ "Sandeep Dasgupta", "Tanmay Gangwani" ] }
    , { arxiv = "1905.08325", title = "Towards Neural Decompilation", authors = [ "Omer Katz", "Yuval Olshaker", "Yoav Goldberg", "Eran Yahav" ] }
    , { arxiv = "1905.08364", title = "Efficient Synthesis with Probabilistic Constraints", authors = [ "Samuel Drews", "Aws Albarghouthi", "Loris D'Antoni" ] }
    , { arxiv = "1905.08921", title = "D2d -- XML for Authors", authors = [ "Markus Lepper", "Baltasar Trancón y Widemann" ] }
    , { arxiv = "1905.09137", title = "A Quick Introduction to Functional Verification of Array-Intensive Programs", authors = [ "Kunal Banerjee", "Chandan Karfa" ] }
    , { arxiv = "1905.09242", title = "Reductions for Automated Hypersafety Verification", authors = [ "Azadeh Farzan", "Anthony Vandikas" ] }
    , { arxiv = "1905.09423", title = "Set Constraints, Pattern Match Analysis, and SMT", authors = [ "Joseph Eremondi" ] }
    , { arxiv = "1905.09610", title = "Hypothetical answers to continuous queries over data streams", authors = [ "Luís Cruz-Filipe", "Graça Gaspar", "Isabel Nunes" ] }
    , { arxiv = "1905.09825", title = "Synthesizing Functional Reactive Programs", authors = [ "Bernd Finkbeiner", "Felix Klein", "Ruzica Piskac", "Mark Santolucito" ] }
    , { arxiv = "1905.09996", title = "Verifying Asynchronous Event-Driven Programs Using Partial Abstract Transformers (Extended Manuscript)", authors = [ "Peizun Liu", "Thomas Wahl", "Akash LaL" ] }
    , { arxiv = "1905.10728", title = "Programming with Applicative-like expressions", authors = [ "Jan Malakhovski", "Sergei Soloviev" ] }
    , { arxiv = "1905.10855", title = "Data Race Prediction for Inaccurate Traces", authors = [ "Martin Sulzmann", "Kai Stadtmüller" ] }
    , { arxiv = "1905.12292", title = "Categorization of Program Regions for Agile Compilation using Machine Learning and Hardware Support", authors = [ "Sanket Tavarageri" ] }
    , { arxiv = "1905.12444", title = "An Experiment with a User Manual of a Programming Language Based on a Denotational Semantics", authors = [ "Andrzej Blikle" ] }
    , { arxiv = "1905.12594", title = "Fuzzi: A Three-Level Logic for Differential Privacy", authors = [ "Hengchu Zhang", "Edo Roth", "Andreas Haeberlen", "Benjamin C. Pierce", "Aaron Roth" ] }
    , { arxiv = "1905.12734", title = "Sub-Turing Islands in the Wild", authors = [ "Earl T. Barr", "David W. Binkley", "Mark Harman", "Mohamed Nassim Seghir" ] }
    , { arxiv = "1905.13674", title = "On the Interaction of Object-Oriented Design Patterns and Programming Languages", authors = [ "Gerald Baumgartner", "Konstantin Läufer", "Vincent F. Russo" ] }
    , { arxiv = "1905.13706", title = "A Role for Dependent Types in Haskell (Extended version)", authors = [ "Stephanie Weirich", "Pritam Choudhury", "Antoine Voizard", "Richard A. Eisenberg" ] }
    , { arxiv = "1905.13716", title = "Reference Capabilities for Safe Parallel Array Programming", authors = [ "Beatrice Ã kerblom", "Elias Castegren", "Tobias Wrigstad" ] }
    , { arxiv = "1905.00922", title = "Typed-based Relaxed Noninterference for Free", authors = [ "Minh Ngo", "David A. Naumann", "Tamara Rezk" ] }
    , { arxiv = "1905.01659", title = "SIF: A Framework for Solidity Code Instrumentation and Analysis", authors = [ "Chao Peng", "Sefa Akca", "Ajitha Rajan" ] }
    , { arxiv = "1905.02158", title = "Parsl: Pervasive Parallel Programming in Python", authors = [ "Yadu Babuji", "Anna Woodard", "Zhuozhao Li", "Daniel S. Katz", "Ben Clifford", "Rohan Kumar", "Lukasz Lacinski", "Ryan Chard", "Justin M. Wozniak", "Ian Foster", "Michael Wilde", "Kyle Chard" ] }
    , { arxiv = "1905.02617", title = "A Type Theory for Defining Logics and Proofs", authors = [ "Brigitte Pientka", "David Thibodeau", "Andreas Abel", "Francisco Ferreira", "Rebecca Zucchini" ] }
    , { arxiv = "1905.04232", title = "Automatic Programming of Cellular Automata and Artificial Neural Networks Guided by Philosophy", authors = [ "Patrik Christen", "Olivier Del Fabbro" ] }
    , { arxiv = "1905.04642", title = "Software System Design based on Patterns for Newton-Type Methods", authors = [ "Ricardo Serrato Barrera", "Gustavo Rodríguez Gómez", "Julio César Pérez Sansalvador", "Saul E. Pomares Hernández", "Leticia Flores Pulido", "Antonio Muñoz" ] }
    , { arxiv = "1905.06233", title = "Generic Encodings of Constructor Rewriting Systems", authors = [ "Horatiu Cirstea", "Pierre-Etienne Moreau" ] }
    , { arxiv = "1905.06707", title = "Inferring Javascript types using Graph Neural Networks", authors = [ "Jessica Schrouff", "Kai Wohlfahrt", "Bruno Marnette", "Liam Atkinson" ] }
    , { arxiv = "1905.06777", title = "Towards Comparing Programming Paradigms", authors = [ "Igor Ivkic", "Alexander Wöhrer", "Markus Tauber" ] }
    , { arxiv = "1905.07653", title = "A Case Study: Exploiting Neural Machine Translation to Translate CUDA to OpenCL", authors = [ "Yonghae Kim", "Hyesoon Kim" ] }
    , { arxiv = "1905.08240", title = "Safe and Chaotic Compilation for Hidden Deterministic Hardware Aliasing", authors = [ "Peter T. Breuer" ] }
    , { arxiv = "1905.08505", title = "Verification Artifacts in Cooperative Verification: Survey and Unifying Component Framework", authors = [ "Dirk Beyer", "Heike Wehrheim" ] }
    , { arxiv = "1905.10065", title = "A Customised App to Attract Female Teenagers to Coding", authors = [ "Bernadette Spieler", "Wolfgang Slany" ] }
    , { arxiv = "1905.11445", title = "COSET: A Benchmark for Evaluating Neural Program Embeddings", authors = [ "Ke Wang", "Mihai Christodorescu" ] }
    , { arxiv = "1905.13334", title = "How Often Do Single-Statement Bugs Occur? The ManySStuBs4J Dataset", authors = [ "Rafael-Michael Karampatsis", "Charles Sutton" ] }
    , { arxiv = "1905.13411", title = "Understanding and Extending Incremental Determinization for 2QBF", authors = [ "Markus N. Rabe", "Leander Tentrup", "Cameron Rasmussen", "Sanjit A. Seshia" ] }
    , { arxiv = "1905.13429", title = "Differential Equation Invariance Axiomatization", authors = [ "André Platzer", "Yong Kiam Tan" ] }
    , { arxiv = "1905.09721", title = "Statistical Assertions for Validating Patterns and Finding Bugs in Quantum Programs", authors = [ "Yipeng Huang", "Margaret Martonosi" ] }
    , { arxiv = "1906.00046", title = "Interaction Trees: Representing Recursive and Impure Programs in Coq", authors = [ "Li-yao Xia", "Yannick Zakowski", "Paul He", "Chung-Kil Hur", "Gregory Malecha", "Benjamin C. Pierce", "Steve Zdancewic" ] }
    , { arxiv = "1906.01706", title = "Unification-based Pointer Analysis without Oversharing", authors = [ "Jakub Kuderski", "Jorge A. Navas", "Arie Gurfinkel" ] }
    , { arxiv = "1906.03937", title = "Java Generics: An Order-Theoretic Approach (Detailed Outline)", authors = [ "Moez A. AbdelGawad" ] }
    , { arxiv = "1906.03957", title = "Type-Driven Automated Learning with Lale", authors = [ "Martin Hirzel", "Kiran Kate", "Avraham Shinnar", "Subhrajit Roy", "Parikshit Ram" ] }
    , { arxiv = "1906.03969", title = "Datalog Disassembly", authors = [ "Antonio Flores-Montoya", "Eric Schulte" ] }
    , { arxiv = "1906.03970", title = "A scheme for dynamically integrating C library functions into a λProlog implementation", authors = [ "Duanyang Jing" ] }
    , { arxiv = "1906.03982", title = "TickTalk -- Timing API for Dynamically Federated Cyber-Physical Systems", authors = [ "Bob Iannucci", "Aviral Shrivastava", "Mohammad Khayatian" ] }
    , { arxiv = "1906.04604", title = "Write, Execute, Assess: Program Synthesis with a REPL", authors = [ "Kevin Ellis", "Maxwell Nye", "Yewen Pu", "Felix Sosa", "Josh Tenenbaum", "Armando Solar-Lezama" ] }
    , { arxiv = "1906.04765", title = "The Prolog debugger and declarative programming", authors = [ "Włodzimierz Drabent" ] }
    , { arxiv = "1906.04830", title = "Polymorphic Relaxed Noninterference", authors = [ "Raimil Cruz", "Éric Tanter" ] }
    , { arxiv = "1906.04924", title = "Lifestate: Event-Driven Protocols and Callback Control Flow (Extended Version)", authors = [ "Shawn Meier", "Sergio Mover", "Bor-Yuh Evan Chang" ] }
    , { arxiv = "1906.04925", title = "Using Category Theory in Modeling Generics in OOP (Outline)", authors = [ "Moez A. AbdelGawad" ] }
    , { arxiv = "1906.04984", title = "SAFEVM: A Safety Verifier for Ethereum Smart Contracts", authors = [ "Elvira Albert", "Jesús Correas", "Pablo Gordillo", "Guillermo Román-Díez", "Albert Rubio" ] }
    , { arxiv = "1906.06469", title = "Approximate Normalization for Gradual Dependent Types", authors = [ "Joseph Eremondi", "Éric Tanter", "Ronald Garcia" ] }
    , { arxiv = "1906.07223", title = "How to Avoid Making a Billion-Dollar Mistake: Type-Safe Data Plane Programming with SafeP4", authors = [ "Matthias Eichholz", "Eric Campbell", "Nate Foster", "Guido Salvaneschi", "Mira Mezini" ] }
    , { arxiv = "1906.07629", title = "The Mathematical Specification of the Statebox Language", authors = [ "Statebox Team", "Fabrizio Genovese", "Jelle Herold" ] }
    , { arxiv = "1906.09503", title = "LNL-FPC: The Linear/Non-linear Fixpoint Calculus", authors = [ "Bert Lindenhovius", "Michael Mislove", "Vladimir Zamdzhiev" ] }
    , { arxiv = "1906.09709", title = "Transitivity of Subtyping for Intersection Types", authors = [ "Jeremy G. Siek" ] }
    , { arxiv = "1906.10204", title = "Automatic verification of heap-manipulating programs", authors = [ "Yurii Kostyukov", "Konstantin Batoev", "Dmitry Mordvinov", "Michael Kostitsyn", "Aleksandr Misonizhnik" ] }
    , { arxiv = "1906.10757", title = "Proceedings Seventh International Workshop on Trends in Functional Programming in Education", authors = [ "Peter Achten", "Heather Miller" ] }
    , { arxiv = "1906.11197", title = "Java Generics: An Order-Theoretic Approach (Abridged Outline)", authors = [ "Moez A. AbdelGawad" ] }
    , { arxiv = "1906.11199", title = "Deployable probabilistic programming", authors = [ "David Tolpin" ] }
    , { arxiv = "1906.11422", title = "Stepping OCaml", authors = [ "Tsukino Furukawa", "Youyou Cong", "Kenichi Asai" ] }
    , { arxiv = "1906.11425", title = "Introducing Certified Compilation in Education by a Functional Language Approach", authors = [ "Per Lindgren", "Marcus Lindner", "Nils Fitinghoff" ] }
    , { arxiv = "1906.11450", title = "Investigating Compilation Errors of Students Learning Haskell", authors = [ "Boldizsár Németh", "Eunjong Choi", "Erina Makihara", "Hajimu Iida" ] }
    , { arxiv = "1906.11606", title = "Structural Contracts -- Contracts for Type Construction & Dependent Types to Ensure Consistency of Extra-Functional Reasoning", authors = [ "Gregor Nitsche" ] }
    , { arxiv = "1906.11929", title = "Invariant Detection with Program Verification Tools", authors = [ "Wei He" ] }
    , { arxiv = "1906.12029", title = "A Neural-based Program Decompiler", authors = [ "Cheng Fu", "Huili Chen", "Haolan Liu", "Xinyun Chen", "Yuandong Tian", "Farinaz Koushanfar", "Jishen Zhao" ] }
    , { arxiv = "1906.12095", title = "Robustness Against Transactional Causal Consistency", authors = [ "Sidi Mohamed Beillahi", "Ahmed Bouajjani", "Constantin Enea" ] }
    , { arxiv = "1906.12098", title = "Category-Theoretic Foundations of \"STCLang: State Thread Composition as a Foundation for Monadic Dataflow Parallelism\"", authors = [ "Sebastian Ertel", "Justus Adam", "Norman A. Rink", "Andrés Goens", "Jeronimo Castrillon" ] }
    , { arxiv = "1906.12242", title = "Bidirectional Type Class Instances (Extended Version)", authors = [ "Koen Pauwels", "Georgios Karachalias", "Michiel Derhaeg", "Tom Schrijvers" ] }
    , { arxiv = "1906.00367", title = "A Survey of Asynchronous Programming Using Coroutines in the Internet of Things and Embedded Systems", authors = [ "Bruce Belson", "Jason Holdsworth", "Wei Xiang", "Bronson Philippa" ] }
    , { arxiv = "1906.00715", title = "On Modelling the Avoidability of Patterns as CSP", authors = [ "Thorsten Ehlers", "Florin Manea", "Dirk Nowotka", "Kamellia Reshadi" ] }
    , { arxiv = "1906.01128", title = "Assessing Performance Implications of Deep Copy Operations via Microbenchmarking", authors = [ "Millad Ghane", "Sunita Chandrasekaran", "Margaret S. Cheung" ] }
    , { arxiv = "1906.01519", title = "Bialgebraic Semantics for String Diagrams", authors = [ "Filippo Bonchi", "Robin Piedeleu", "Pawel Sobocinski", "Fabio Zanasi" ] }
    , { arxiv = "1906.03836", title = "Minimal Session Types (Extended Version)", authors = [ "Alen Arslanagić", "Jorge A. Pérez", "Erik Voogd" ] }
    , { arxiv = "1906.04011", title = "Visual Backpropagation", authors = [ "Roy S. Freedman" ] }
    , { arxiv = "1906.04908", title = "SPoC: Search-based Pseudocode to Code", authors = [ "Sumith Kulal", "Panupong Pasupat", "Kartik Chandra", "Mina Lee", "Oded Padon", "Alex Aiken", "Percy Liang" ] }
    , { arxiv = "1906.05092", title = "Migrating large codebases to C++ Modules", authors = [ "Yuka Takahashi", "Oksana Shadura", "Vassil Vassilev" ] }
    , { arxiv = "1906.05170", title = "Efficient Graph Rewriting", authors = [ "Graham Campbell" ] }
    , { arxiv = "1906.05704", title = "Modeling and Verifying Cyber-Physical Systems with Hybrid Active Objects", authors = [ "Eduard Kamburjan", "Stefan Mitsch", "Martina Kettenbach", "Reiner Hähnle" ] }
    , { arxiv = "1906.07181", title = "Learning Execution through Neural Code Fusion", authors = [ "Zhan Shi", "Kevin Swersky", "Daniel Tarlow", "Parthasarathy Ranganathan", "Milad Hashemi" ] }
    , { arxiv = "1906.08911", title = "Toward a Standard Interface for User-Defined Scheduling in OpenMP", authors = [ "Vivek Kale", "Christian Iwainsky", "Michael Klemm", "Jonas H. Muller Korndorfer", "Florina M. Ciorba" ] }
    , { arxiv = "1906.09370", title = "Strong Bisimulation for Control Operators", authors = [ "Eduardo Bonelli", "Delia Kesner", "Andrés Viso" ] }
    , { arxiv = "1906.09702", title = "Heterogeneous Active Messages (HAM) -- Implementing Lightweight Remote Procedure Calls in C++", authors = [ "Matthias Noack" ] }
    , { arxiv = "1906.10502", title = "SampleFix: Learning to Correct Programs by Sampling Diverse Fixes", authors = [ "Hossein Hajipour", "Apratim Bhattacharya", "Mario Fritz" ] }
    , { arxiv = "1906.10719", title = "A unifying framework for continuity and complexity in higher types", authors = [ "Thomas Powell" ] }
    , { arxiv = "1906.10811", title = "Investigating the OPS intermediate representation to target GPUs in the Devito DSL", authors = [ "Vincenzo Pandolfo" ] }
    , { arxiv = "1906.10816", title = "Program Synthesis and Semantic Parsing with Learned Code Idioms", authors = [ "Richard Shin", "Miltiadis Allamanis", "Marc Brockschmidt", "Oleksandr Polozov" ] }
    , { arxiv = "1906.11421", title = "FSM Error Messages", authors = [ "Marco T. Morazán", "Josephine A. Des Rosiers" ] }
    , { arxiv = "1906.11423", title = "Vector Programming Using Generative Recursion", authors = [ "Marco T. Morazán" ] }
    , { arxiv = "1906.12066", title = "Pinpointing Performance Inefficiencies in Java", authors = [ "Pengfei Su", "Qingsen Wang", "Milind Chabbi", "Xu Liu" ] }
    , { arxiv = "1906.03028", title = "Automatic Reparameterisation of Probabilistic Programs", authors = [ "Maria I. Gorinova", "Dave Moore", "Matthew D. Hoffman" ] }
    , { arxiv = "1907.00298", title = "Deciding Memory Safety for Single-Pass Heap-Manipulating Programs", authors = [ "Umang Mathur", "Adithya Murali", "Paul Krogmeier", "P. Madhusudan", "Mahesh Viswanathan" ] }
    , { arxiv = "1907.00421", title = "A Sound Algorithm for Asynchronous Session Subtyping and its Implementation", authors = [ "Mario Bravetti", "Marco Carbone", "Julien Lange", "Nobuko Yoshida", "Gianluigi Zavattaro" ] }
    , { arxiv = "1907.00509", title = "The Semantics of Rank Polymorphism", authors = [ "Justin Slepak", "Olin Shivers", "Panagiotis Manolios" ] }
    , { arxiv = "1907.00822", title = "On consistency types for lattice-based distributed programming languages", authors = [ "Xin Zhao", "Philipp Haller" ] }
    , { arxiv = "1907.00844", title = "Coherence of Type Class Resolution", authors = [ "Gert-Jan Bottu", "Ningning Xie", "Koar Marntirosian", "Tom Schrijvers" ] }
    , { arxiv = "1907.00855", title = "Type Checking Program Code using SHACL (Extended Version)", authors = [ "Martin Leinberger", "Philipp Seifer", "Claudia Schon", "Ralf Lämmel", "Steffen Staab" ] }
    , { arxiv = "1907.00863", title = "Understanding GCC Builtins to Develop Better Tools", authors = [ "Manuel Rigger", "Stefan Marr", "Bram Adams", "Hanspeter Mössenböck" ] }
    , { arxiv = "1907.01257", title = "Local Reasoning for Robust Observational Equivalence", authors = [ "Dan R. Ghica", "Koko Muroya", "Todd Waugh Ambridge" ] }
    , { arxiv = "1907.02558", title = "Integration of the Static Analysis Results Interchange Format in CogniCrypt", authors = [ "Sriteja Kummita", "Goran Piskachev" ] }
    , { arxiv = "1907.02859", title = "GTIRB: Intermediate Representation for Binaries", authors = [ "Eric Schulte", "Jonathan Dorn", "Antonio Flores-Montoya", "Aaron Ballman", "Tom Johnson" ] }
    , { arxiv = "1907.02952", title = "Solidity 0.5: when typed does not mean type safe", authors = [ "Silvia Crafa", "Matteo Di Pirro" ] }
    , { arxiv = "1907.02990", title = "Type-safe, Polyvariadic Event Correlation", authors = [ "Oliver Bračevac", "Guido Salvaneschi", "Sebastian Erdweg", "Mira Mezini" ] }
    , { arxiv = "1907.03105", title = "Constraint-Based Type-Directed Program Synthesis", authors = [ "Peter-Michael Osera" ] }
    , { arxiv = "1907.03436", title = "parboiled2: a macro-based approach for effective generators of parsing expressions grammars in Scala", authors = [ "Alexander A. Myltsev" ] }
    , { arxiv = "1907.03536", title = "A Compositional Framework for Scientific Model Augmentation", authors = [ "Micah Halter", "Christine Herlihy", "James Fairbanks" ] }
    , { arxiv = "1907.03997", title = "Relational Verification via Invariant-Guided Synchronization", authors = [ "Qi Zhou", "David Heath", "William Harris" ] }
    , { arxiv = "1907.04134", title = "A Bridge Anchored on Both Sides: Formal Deduction in Introductory CS, and Code Proofs in Discrete Math", authors = [ "David G. Wonnacott", "Peter-Michael Osera" ] }
    , { arxiv = "1907.04241", title = "CHOP: Bypassing Runtime Bounds Checking Through Convex Hull OPtimization", authors = [ "Yurong Chen", "Hongfa Xue", "Tian Lan", "Guru Venkataramani" ] }
    , { arxiv = "1907.04243", title = "The Combinatorics of Barrier Synchronization", authors = [ "Olivier Bodini", "Matthieu Dien", "Antoine Genitrini", "Frédéric Peschanski" ] }
    , { arxiv = "1907.04262", title = "solc-verify: A Modular Verifier for Solidity Smart Contracts", authors = [ "Ákos Hajdu", "Dejan Jovanović" ] }
    , { arxiv = "1907.04934", title = "CallE: An Effect System for Method Calls", authors = [ "Isaac Oscar Gariano", "James Noble", "Marco Servetto" ] }
    , { arxiv = "1907.05045", title = "Provenance for Large-scale Datalog", authors = [ "David Zhao", "Pavle Subotic", "Bernhard Scholz" ] }
    , { arxiv = "1907.05118", title = "R Melts Brains -- An IR for First-Class Environments and Lazy Effectful Arguments", authors = [ "Olivier Flückiger", "Guido Chari", "Jan Ječmen", "Ming-Ho Yee", "Jakob Hain", "Jan Vitek" ] }
    , { arxiv = "1907.05244", title = "The Next 700 Relational Program Logics", authors = [ "Kenji Maillard", "Catalin Hritcu", "Exequiel Rivas", "Antoine Van Muylder" ] }
    , { arxiv = "1907.05308", title = "Executable formal semantics for the POSIX shell", authors = [ "Michael Greenberg", "Austin J. Blatt" ] }
    , { arxiv = "1907.05320", title = "Trace-Relating Compiler Correctness and Secure Compilation", authors = [ "Carmine Abate", "Roberto Blanco", "Stefan Ciobaca", "Adrien Durier", "Deepak Garg", "Catalin Hritcu", "Marco Patrignani", "Éric Tanter", "Jérémy Thibault" ] }
    , { arxiv = "1907.05451", title = "Compositional Inference Metaprogramming with Convergence Guarantees", authors = [ "Shivam Handa", "Vikash Mansinghka", "Martin Rinard" ] }
    , { arxiv = "1907.05590", title = "Revisiting Occurrence Typing", authors = [ "Giuseppe Castagna", "Victor Lanvin", "Mickaël Laurent", "Kim Nguyen" ] }
    , { arxiv = "1907.05637", title = "Concolic Testing Heap-Manipulating Programs", authors = [ "Long H. Pham", "Quang Loc Le", "Quoc-Sang Phan", "Jun Sun" ] }
    , { arxiv = "1907.05649", title = "Augmenting Type Signatures for Program Synthesis", authors = [ "Bruce Collie", "Michael O'Boyle" ] }
    , { arxiv = "1907.05818", title = "Verified Self-Explaining Computation", authors = [ "Jan Stolarek", "James Cheney" ] }
    , { arxiv = "1907.05871", title = "Phoenix -- The Arabic Object-Oriented Programming Language", authors = [ "Youssef Bassil" ] }
    , { arxiv = "1907.05873", title = "Compiler Design for Legal Document Translation in Digital Government", authors = [ "Youssef Bassil" ] }
    , { arxiv = "1907.06249", title = "Bayesian Synthesis of Probabilistic Programs for Automatic Data Modeling", authors = [ "Feras A. Saad", "Marco F. Cusumano-Towner", "Ulrich Schaechtle", "Martin C. Rinard", "Vikash K. Mansinghka" ] }
    , { arxiv = "1907.06522", title = "A Relational Static Semantics for Call Graph Construction", authors = [ "Xilong Zhuo", "Chenyi Zhang" ] }
    , { arxiv = "1907.07283", title = "Recovering Purity with Comonads and Capabilities", authors = [ "Vikraman Choudhury", "Neel Krishnaswami" ] }
    , { arxiv = "1907.07587", title = "A Differentiable Programming System to Bridge Machine Learning and Scientific Computing", authors = [ "Mike Innes", "Alan Edelman", "Keno Fischer", "Chris Rackauckas", "Elliot Saba", "Viral B Shah", "Will Tebbutt" ] }
    , { arxiv = "1907.07764", title = "HTCC: Haskell to Handel-C Compiler", authors = [ "Ahmed Ablak", "Issam Damaj" ] }
    , { arxiv = "1907.07794", title = "Generating Correctness Proofs with Neural Networks", authors = [ "Alex Sanchez-Stern", "Yousef Alhessi", "Lawrence Saul", "Sorin Lerner" ] }
    , { arxiv = "1907.08003", title = "Asynchronous Snapshots of Actor Systems for Latency-Sensitive Applications", authors = [ "Dominik Aumayr", "Stefan Marr", "Elisa Gonzalez Boix", "Hanspeter Mössenböck" ] }
    , { arxiv = "1907.08251", title = "Responsibility Analysis by Abstract Interpretation", authors = [ "Chaoqiang Deng", "Patrick Cousot" ] }
    , { arxiv = "1907.08695", title = "Language Support for Adaptation: Intent-Driven Programming in FAST", authors = [ "Yao-Hsiang Yang", "Adam Duracz", "Ferenc A. Bartha", "Ryuichi Sai", "Ahsan Pervaiz", "Saeid Barati", "Dung Nguyen", "Robert Cartwright", "Henry Hoffmann", "Krishna V. Palem" ] }
    , { arxiv = "1907.08827", title = "Towards Verified Stochastic Variational Inference for Probabilistic Programs", authors = [ "Wonyeol Lee", "Hangyeol Yu", "Xavier Rival", "Hongseok Yang" ] }
    , { arxiv = "1907.08834", title = "Towards meta-interpretive learning of programming language semantics", authors = [ "Sándor Bartha", "James Cheney" ] }
    , { arxiv = "1907.09820", title = "The Expressive Power of Higher-Order Datalog", authors = [ "Angelos Charalambidis", "Christos Nomikos", "Panos Rondogiannis" ] }
    , { arxiv = "1907.10096", title = "Resource Analysis driven by (Conditional) Termination Proofs", authors = [ "Elvira Albert", "Miquel Bofill", "Cristina Borralleras", "Enrique Martin-Martin", "Albert Rubio" ] }
    , { arxiv = "1907.10278", title = "A Case for Stale Synchronous Distributed Model for Declarative Recursive Computation", authors = [ "Ariyam Das", "Carlo Zaniolo" ] }
    , { arxiv = "1907.10674", title = "ConCert: A Smart Contract Certification Framework in Coq", authors = [ "Danil Annenkov", "Jakob Botsch Nielsen", "Bas Spitters" ] }
    , { arxiv = "1907.10708", title = "A Probabilistic Separation Logic", authors = [ "Gilles Barthe", "Justin Hsu", "Kevin Liao" ] }
    , { arxiv = "1907.10922", title = "Spacetime Programming: A Synchronous Language for Composable Search Strategies", authors = [ "Pierre Talbot" ] }
    , { arxiv = "1907.11133", title = "An Introduction to Logical Relations", authors = [ "Lau Skorstengaard" ] }
    , { arxiv = "1907.11317", title = "Explicit and Controllable Assignment Semantics", authors = [ "Dimitri Racordon", "Didier Buchs" ] }
    , { arxiv = "1907.11354", title = "Lazy Stream Programming in Prolog", authors = [ "Paul Tarau", "Jan Wielemaker", "Tom Schrijvers" ] }
    , { arxiv = "1907.12345", title = "Control-Flow Refinement by Partial Evaluation, and its Application to Termination and Cost Analysis", authors = [ "Jesús J. Doménech", "John P. Gallagher", "Samir Genaim" ] }
    , { arxiv = "1907.13263", title = "Computing Abstract Distances in Logic Programs", authors = [ "Ignacio Casso", "Jose F. Morales", "Pedro Lopez-Garcia", "Manuel V. Hermenegildo" ] }
    , { arxiv = "1907.13272", title = "Towards a General Framework for Static Cost Analysis of Parallel Logic Programs", authors = [ "Maximiliano Klemen", "Pedro Lopez-Garcia", "John P. Gallagher", "Jose F. Morales", "Manuel V. Hermenegildo" ] }
    , { arxiv = "1907.00713", title = "Verifying that a compiler preserves concurrent value-dependent information-flow security", authors = [ "Robert Sison", "Toby Murray" ] }
    , { arxiv = "1907.01297", title = "Neural Network Verification for the Masses (of AI graduates)", authors = [ "Ekaterina Komendantskaya", "Rob Stewart", "Kirsy Duncan", "Daniel Kienitz", "Pierre Le Hen", "Pascal Bacchus" ] }
    , { arxiv = "1907.01727", title = "Uncovering Information Flow Policy Violations in C Programs", authors = [ "Darion Cassel", "Yan Huang", "Limin Jia" ] }
    , { arxiv = "1907.02064", title = "Accelerator-level Parallelism", authors = [ "Mark D. Hill", "Vijay Janapa Reddi" ] }
    , { arxiv = "1907.02192", title = "Lifting Datalog-Based Analyses to Software Product Lines", authors = [ "Ramy Shahin", "Marsha Chechik", "Rick Salay" ] }
    , { arxiv = "1907.02597", title = "Multi-dimensional interpolations in C++", authors = [ "Maarten de Jong" ] }
    , { arxiv = "1907.03996", title = "Understanding Counterexamples for Relational Properties with DIbugger", authors = [ "Mihai Herda", "Michael Kirsten", "Etienne Brunner", "Joana Plewnia", "Ulla Scheler", "Chiara Staudenmaier", "Benedikt Wagner", "Pascal Zwick", "Bernhard Beckert" ] }
    , { arxiv = "1907.03999", title = "Proving Properties of Sorting Programs: A Case Study in Horn Clause Verification", authors = [ "Emanuele De Angelis", "Fabio Fioravanti", "Alberto Pettorossi", "Maurizio Proietti" ] }
    , { arxiv = "1907.05431", title = "Imitation-Projected Programmatic Reinforcement Learning", authors = [ "Abhinav Verma", "Hoang M. Le", "Yisong Yue", "Swarat Chaudhuri" ] }
    , { arxiv = "1907.05690", title = "Mercem: Method Name Recommendation Based on Call Graph Embedding", authors = [ "Hiroshi Yonai", "Yasuhiro Hayase", "Hiroyuki Kitagawa" ] }
    , { arxiv = "1907.05920", title = "Guarded Kleene Algebra with Tests: Verification of Uninterpreted Programs in Nearly Linear Time", authors = [ "Steffen Smolka", "Nate Foster", "Justin Hsu", "Tobias Kappé", "Dexter Kozen", "Alexandra Silva" ] }
    , { arxiv = "1907.06057", title = "Crumbling Abstract Machines", authors = [ "Beniamino Accattoli", "Andrea Condoluci", "Giulio Guerrieri", "Claudio Sacerdoti Coen" ] }
    , { arxiv = "1907.06205", title = "Automatic Repair and Type Binding of Undeclared Variables using Neural Networks", authors = [ "Venkatesh Theru Mohan", "Ali Jannesari" ] }
    , { arxiv = "1907.07154", title = "Object-Capability as a Means of Permission and Authority in Software Systems", authors = [ "Jörn Koepe" ] }
    , { arxiv = "1907.07761", title = "Runtime Verification For Timed Event Streams With Partial Information", authors = [ "Martin Leucker", "César Sánchez", "Torben Scheffel", "Malte Schmitz", "Daniel Thoma" ] }
    , { arxiv = "1907.08337", title = "Online Set-Based Dynamic Analysis for Sound Predictive Race Detection", authors = [ "Jake Roemer", "Michael D. Bond" ] }
    , { arxiv = "1907.09282", title = "Learning the Relation between Code Features and Code Transforms with Structured Prediction", authors = [ "Zhongxing Yu", "Matias Martinez", "Tegawendé F. Bissyandé", "Martin Monperrus" ] }
    , { arxiv = "1907.10662", title = "ART: Abstraction Refinement-Guided Training for Provably Correct Neural Networks", authors = [ "Xuankang Lin", "He Zhu", "Roopsha Samanta", "Suresh Jagannathan" ] }
    , { arxiv = "1907.10699", title = "Sketch-n-Sketch: Output-Directed Programming for SVG", authors = [ "Brian Hempel", "Justin Lubin", "Ravi Chugh" ] }
    , { arxiv = "1907.10914", title = "Applying Constraint Logic Programming to SQL Semantic Analysis", authors = [ "Fernando Sáenz-Pérez" ] }
    , { arxiv = "1907.10919", title = "Symbolic Analysis of Maude Theories with Narval", authors = [ "María Alpuente", "Demis Ballis", "Santiago Escobar", "Julia Sapiña" ] }
    , { arxiv = "1907.11817", title = "Scalable Source Code Similarity Detection in Large Code Repositories", authors = [ "F Alomari", "M Harbi" ] }
    , { arxiv = "1907.13227", title = "Compiling With Classical Connectives", authors = [ "Paul Downen", "Zena M. Ariola" ] }
    , { arxiv = "1907.13288", title = "VISCR: Intuitive & Conflict-free Automation for Securing the Dynamic Consumer IoT Infrastructures", authors = [ "Vasudevan Nagendra", "Arani Bhattacharya", "Vinod Yegneswaran", "Amir Rahmati", "Samir R Das" ] }
    , { arxiv = "1908.00093", title = "Aquarium: Cassiopea and Alewife Languages", authors = [ "David A. Holland", "Jingmei Hu", "Ming Kawaguchi", "Eric Lu", "Stephen Chong", "Margo I. Seltzer" ] }
    , { arxiv = "1908.00104", title = "Evaluation of the Implementation of an Abstract Interpretation Algorithm using Tabled CLP", authors = [ "Joaquin Arias", "Manuel Carro" ] }
    , { arxiv = "1908.00441", title = "Refinement Kinds: Type-safe Programming with Practical Type-level Computation (Extended Version)", authors = [ "Luís Caires", "Bernardo Toninho" ] }
    , { arxiv = "1908.00898", title = "The meaning of a program change is a change to the program's meaning", authors = [ "Roly Perera" ] }
    , { arxiv = "1908.01057", title = "Proposition d'un modèle pour l'optimisation automatique de boucles dans le compilateur Tiramisu : cas d'optimisation de déroulage", authors = [ "Asma Balamane", "Zina Taklit" ] }
    , { arxiv = "1908.01324", title = "CREST: Hardware Formal Verification with ANSI-C Reference Specifications", authors = [ "Andreas Tiemeyer", "Tom Melham", "Daniel Kroening", "John O'Leary" ] }
    , { arxiv = "1908.01572", title = "Examples and Results from a BSc-level Course on Domain Specific Languages of Mathematics", authors = [ "Patrik Jansson", "Sólrún Halla Einarsdóttir", "Cezar Ionescu" ] }
    , { arxiv = "1908.02035", title = "A Dependently Typed Multi-Stage Calculus", authors = [ "Akira Kawata", "Atsushi Igarashi" ] }
    , { arxiv = "1908.02078", title = "A Transformational Approach to Resource Analysis with Typed-norms Inference", authors = [ "Elvira Albert", "Samir Genaim", "Raúl Gutiérrez", "Enrique Martin-Martin" ] }
    , { arxiv = "1908.02414", title = "Space-Efficient Gradual Typing in Coercion-Passing Style", authors = [ "Yuya Tsuda", "Atsushi Igarashi", "Tomoya Tabuchi" ] }
    , { arxiv = "1908.02709", title = "A minimal core calculus for Solidity contracts", authors = [ "Massimo Bartoletti", "Letterio Galletta", "Maurizio Murgia" ] }
    , { arxiv = "1908.02940", title = "Intrinsically-Typed Mechanized Semantics for Session Types", authors = [ "Peter Thiemann" ] }
    , { arxiv = "1908.03010", title = "Manifest Contracts with Intersection Types", authors = [ "Yuki Nishida", "Atsushi Igarashi" ] }
    , { arxiv = "1908.03316", title = "Multi-Modal Synthesis of Regular Expressions", authors = [ "Qiaochu Chen", "Xinyu Wang", "Xi Ye", "Greg Durrett", "Isil Dillig" ] }
    , { arxiv = "1908.03619", title = "Functional programming with lambda-tree syntax", authors = [ "Ulysse Gérard", "Dale Miller", "Gabriel Scherer" ] }
    , { arxiv = "1908.04265", title = "Recursion, Probability, Convolution and Classification for Computations", authors = [ "Mircea Namolaru", "Thierry Goubier" ] }
    , { arxiv = "1908.04546", title = "Type-Directed Program Synthesis and Constraint Generation for Library Portability", authors = [ "Bruce Collie", "Philip Ginsbach", "Michael F.P. O'Boyle" ] }
    , { arxiv = "1908.05294", title = "Undecidability of D<: and Its Decidable Fragments", authors = [ "Jason Hu", "Ondřej Lhoták" ] }
    , { arxiv = "1908.05535", title = "Toward Structured Proofs for Dynamic Logics", authors = [ "Brandon Bohrer", "André Platzer" ] }
    , { arxiv = "1908.05647", title = "Counting Immutable Beans: Reference Counting Optimized for Purely Functional Programming", authors = [ "Sebastian Ullrich", "Leonardo de Moura" ] }
    , { arxiv = "1908.05655", title = "CLOTHO: Directed Test Generation for Weakly Consistent Database Systems", authors = [ "Kia Rahmani", "Kartik Nagar", "Benjamin Delaware", "Suresh Jagannathan" ] }
    , { arxiv = "1908.05799", title = "Modular Verification of Heap Reachability Properties in Separation Logic", authors = [ "Arshavir Ter-Gabrielyan", "Alexander J. Summers", "Peter Müller" ] }
    , { arxiv = "1908.05839", title = "Bidirectional Typing", authors = [ "Joshua Dunfield", "Neel Krishnaswami" ] }
    , { arxiv = "1908.05845", title = "Memory-Efficient Object-Oriented Programming on GPUs", authors = [ "Matthias Springer" ] }
    , { arxiv = "1908.06478", title = "Type-Based Resource Analysis on Haskell", authors = [ "Franz Siglmüller" ] }
    , { arxiv = "1908.06601", title = "Implicit Recursive Characteristics of STOP", authors = [ "Mike H. Ji" ] }
    , { arxiv = "1908.06723", title = "Proceedings Seventh International Workshop on Verification and Program Transformation", authors = [ "Alexei Lisitsa", "Andrei Nemytykh" ] }
    , { arxiv = "1908.07563", title = "Reactive Probabilistic Programming", authors = [ "Guillaume Baudart", "Louis Mandel", "Eric Atkinson", "Benjamin Sherman", "Marc Pouzet", "Michael Carbin" ] }
    , { arxiv = "1908.07776", title = "Free Theorems Simply, via Dinaturality", authors = [ "Janis Voigtländer" ] }
    , { arxiv = "1908.07883", title = "Scala Implicits are Everywhere: A large-scale study of the use of Implicits in the wild", authors = [ "Filip Křikava", "Heather Miller", "Jan Vitek" ] }
    , { arxiv = "1908.09123", title = "Dependent Pearl: Normalization by realizability", authors = [ "Pierre-Évariste Dagand", "Lionel Rieg", "Gabriel Scherer" ] }
    , { arxiv = "1908.09681", title = "Kindly Bent to Free Us", authors = [ "Gabriel Radanne", "Peter Thiemann" ] }
    , { arxiv = "1908.09758", title = "Automated Verification of CountDownLatch", authors = [ "Wei-Ngan Chin", "Ton Chanh Le", "Shengchao Qin" ] }
    , { arxiv = "1908.10051", title = "Compositional Verification of Heap-Manipulating Programs through Property-Guided Learning", authors = [ "Long H. Pham", "Jun Sun", "Quang Loc Le" ] }
    , { arxiv = "1908.10264", title = "Structured Traversal of Search Trees in Constraint-logic Object-oriented Programming", authors = [ "Jan C. Dageförde", "Finn Teegen" ] }
    , { arxiv = "1908.10273", title = "TxForest: A DSL for Concurrent Filestores", authors = [ "Jonathan DiLorenzo", "Katie Mancini", "Kathleen Fisher", "Nate Foster" ] }
    , { arxiv = "1908.10607", title = "Adding Data to Curry", authors = [ "Michael Hanus", "Finn Teegen" ] }
    , { arxiv = "1908.10926", title = "Performance Analysis of Zippers", authors = [ "Vít Šefl" ] }
    , { arxiv = "1908.11101", title = "ICurry", authors = [ "Sergio Antoy", "Michael Hanus", "Andy Jost", "Steven Libby" ] }
    , { arxiv = "1908.11142", title = "Improving the Performance of the Paisley Pattern-Matching EDSL by Staged Combinatorial Compilation", authors = [ "Baltasar Trancón y Widemann", "Markus Lepper" ] }
    , { arxiv = "1908.11227", title = "VeriSmart: A Highly Precise Safety Verifier for Ethereum Smart Contracts", authors = [ "Sunbeom So", "Myungho Lee", "Jisu Park", "Heejo Lee", "Hakjoo Oh" ] }
    , { arxiv = "1908.11343", title = "Modular Runtime Complexity Analysis of Probabilistic While Programs", authors = [ "Martin Avanzini", "Michael Schaper", "Georg Moser" ] }
    , { arxiv = "1908.01909", title = "Circular Proofs as Session-Typed Processes: A Local Validity Condition", authors = [ "Farzaneh Derakhshan", "Frank Pfenning" ] }
    , { arxiv = "1908.03719", title = "Introduction to the 35th International Conference on Logic Programming Special Issue", authors = [ "Esra Erdem", "Andrea Formisano", "German Vidal", "Fangkai Yang" ] }
    , { arxiv = "1908.04291", title = "The far side of the cube", authors = [ "Dan R. Ghica" ] }
    , { arxiv = "1908.04509", title = "On the Complexity of Checking Transactional Consistency", authors = [ "Ranadeep Biswas", "Constantin Enea" ] }
    , { arxiv = "1908.04921", title = "On the Elementary Affine Lambda-Calculus with and Without Fixed Points", authors = [ "Lê Thành Dũng Nguyen" ] }
    , { arxiv = "1908.04922", title = "Pointers in Recursion: Exploring the Tropics", authors = [ "Paulin Jacobé de Naurois" ] }
    , { arxiv = "1908.04923", title = "Type-two Iteration with Bounded Query Revision", authors = [ "Bruce M. Kapron", "Florian Steinberg" ] }
    , { arxiv = "1908.05979", title = "A Gentzen-style monadic translation of Gödel's System T", authors = [ "Chuangjie Xu" ] }
    , { arxiv = "1908.06223", title = "A Symbolic Neural Network Representation and its Application to Understanding, Verifying, and Patching Networks", authors = [ "Matthew Sotoudeh", "Aditya V. Thakur" ] }
    , { arxiv = "1908.07188", title = "Lemma Generation for Horn Clause Satisfiability: A Preliminary Study", authors = [ "Emanuele De Angelis", "Fabio Fioravanti", "Alberto Pettorossi", "Maurizio Proietti" ] }
    , { arxiv = "1908.07189", title = "Polyvariant Program Specialisation with Property-based Abstraction", authors = [ "John P. Gallagher" ] }
    , { arxiv = "1908.08213", title = "Proceedings Combined 26th International Workshop on Expressiveness in Concurrency and 16th Workshop on Structural Operational Semantics", authors = [ "Jorge A. Pérez", "Jurriaan Rot" ] }
    , { arxiv = "1908.09302", title = "Proceedings of the Second Workshop on Verification of Objects at RunTime EXecution", authors = [ "Davide Ancona", "Gordon Pace" ] }
    , { arxiv = "1908.10041", title = "SNITCH: Dynamic Dependent Information Flow Analysis for Independent Java Bytecode", authors = [ "Eduardo Geraldo", "João Costa Seco" ] }
    , { arxiv = "1908.10416", title = "A Type-Based HFL Model Checking Algorithm", authors = [ "Youkichi Hosoi", "Naoki Kobayashi", "Takeshi Tsukada" ] }
    , { arxiv = "1908.10743", title = "On Distributed Runtime Verification by Aggregate Computing", authors = [ "Giorgio Audrito", "Ferruccio Damiani", "Volker Stolz", "Mirko Viroli" ] }
    , { arxiv = "1908.11105", title = "FunSeqSet: Towards a Purely Functional Data Structure for the Linearisation Case of Dynamic Trees Problem", authors = [ "Juan Carlos Saenz-Carrasco" ] }
    , { arxiv = "1908.11169", title = "Cellular Monads from Positive GSOS Specifications", authors = [ "Tom Hirschowitz" ] }
    , { arxiv = "1908.11685", title = "Using LSTMs to Model the Java Programming Language", authors = [ "Brendon Boldt" ] }
    , { arxiv = "1908.11781", title = "AccD: A Compiler-based Framework for Accelerating Distance-related Algorithms on CPU-FPGA Platforms", authors = [ "Yuke Wang", "Boyuan Feng", "Gushu Li", "Lei Deng", "Yuan Xie", "Yufei Ding" ] }
    , { arxiv = "1908.11850", title = "MOD: Minimally Ordered Durable Datastructures for Persistent Memory", authors = [ "Swapnil Haria", "Mark D. Hill", "Michael M. Swift" ] }
    , { arxiv = "1908.02644", title = "Sized Types for low-level Quantum Metaprogramming", authors = [ "Matthew Amy" ] }
    , { arxiv = "1908.08963", title = "Contract-based verification of a realistic quantum compiler", authors = [ "Yunong Shi", "Xupeng Li", "Runzhou Tao", "Ali Javadi-Abhari", "Andrew W. Cross", "Frederic T. Chong", "Ronghui Gu" ] }
    , { arxiv = "1909.00043", title = "Declarative Programming for Microcontrollers -- Datalog on Arduino", authors = [ "Mario Wenzel", "Stefan Brass" ] }
    , { arxiv = "1909.00097", title = "VST-A: A Foundationally Sound Annotation Verifier", authors = [ "Qinshi Wang", "Qinxiang Cao" ] }
    , { arxiv = "1909.00989", title = "Value-centric Dynamic Partial Order Reduction", authors = [ "Krishnendu Chatterjee", "Andreas Pavlogiannis", "Viktor Toman" ] }
    , { arxiv = "1909.01465", title = "Towards Gradual Checking of Reference Capabilities", authors = [ "Kiko Fernandez-Reyes", "Isaac Oscar Gariano", "James Noble", "Tobias Wrigstad" ] }
    , { arxiv = "1909.02457", title = "QCOR: A Language Extension Specification for the Heterogeneous Quantum-Classical Model of Computation", authors = [ "Tiffany M. Mintz", "Alexander J. Mccaskey", "Eugene F. Dumitrescu", "Shirley V. Moore", "Sarah Powers", "Pavel Lougovski" ] }
    , { arxiv = "1909.02481", title = "Duet: An Expressive Higher-order Language and Linear Type System for Statically Enforcing Differential Privacy", authors = [ "Joseph P. Near", "David Darais", "Chike Abuah", "Tim Stevens", "Pranav Gaddamadugu", "Lun Wang", "Neel Somani", "Mu Zhang", "Nikhil Sharma", "Alex Shan", "Dawn Song" ] }
    , { arxiv = "1909.02599", title = "Formal Methods and Event Notification Systems in Mobile Computing Environment", authors = [ "Prashant Kumar", "R. K. Ghosh" ] }
    , { arxiv = "1909.03289", title = "Predicting All Data Race Pairs for a Specific Schedule (extended version)", authors = [ "Martin Sulzmann", "Kai Stadtmüller" ] }
    , { arxiv = "1909.03523", title = "Obsidian: Typestate and Assets for Safer Blockchain Programming", authors = [ "Michael Coblenz", "Reed Oei", "Tyler Etzel", "Paulette Koronkevich", "Miles Baker", "Yannick Bloem", "Brad A. Myers", "Joshua Sunshine", "Jonathan Aldrich" ] }
    , { arxiv = "1909.03658", title = "Sindarin: A Versatile Scripting API for the Pharo Debugger", authors = [ "Thomas Dupriez", "Guillermo Polito", "Steven Costiou", "Vincent Aranega", "Stéphane Ducasse" ] }
    , { arxiv = "1909.03721", title = "CISE3: Verificação de aplicações com consistência fraca em Why3", authors = [ "Filipe Meirim", "Mário Pereira", "Carla Ferreira" ] }
    , { arxiv = "1909.03741", title = "Análise de Segurança Baseada em Roles para Fábricas de Software", authors = [ "Miguel Loureiro", "Luísa Lourenço", "Lúcio Ferrão", "Carla Ferreira" ] }
    , { arxiv = "1909.04160", title = "Structural and semantic pattern matching analysis in Haskell", authors = [ "Pavel Kalvoda", "Tom Sydney Kerckhove" ] }
    , { arxiv = "1909.04374", title = "Cache Persistence Analysis: Finally Exact", authors = [ "Gregory Stock", "Sebastian Hahn", "Jan Reineke" ] }
    , { arxiv = "1909.04870", title = "Pre-proceedings of the DECLARE 2019 Conference", authors = [ "Salvador Abreu", "Petra Hofstedt", "Ulrich John", "Herbert Kuchen", "Dietmar Seipel" ] }
    , { arxiv = "1909.05027", title = "The Marriage of Univalence and Parametricity", authors = [ "Nicolas Tabareau", "Éric Tanter", "Matthieu Sozeau" ] }
    , { arxiv = "1909.05076", title = "Static Analysis for Probabilistic Programs", authors = [ "Ryan Bernstein" ] }
    , { arxiv = "1909.05242", title = "Proceedings 12th Interaction and Concurrency Experience", authors = [ "Massimo Bartoletti", "Ludovic Henrio", "Anastasia Mavridou", "Alceste Scalas" ] }
    , { arxiv = "1909.05339", title = "Floorplan: Spatial Layout in Memory Management Systems", authors = [ "Karl Cronburg", "Samuel Z. Guyer" ] }
    , { arxiv = "1909.05464", title = "A Formal Semantics of Findel in Coq (Short Paper)", authors = [ "Andrei Arusoaie" ] }
    , { arxiv = "1909.05581", title = "Which of My Transient Type Checks Are Not (Almost) Free?", authors = [ "Isaac Oscar Gariano", "Richard Roberts", "Stefan Marr", "Michael Homer", "James Noble" ] }
    , { arxiv = "1909.05951", title = "Deterministic Parallel Fixpoint Computation", authors = [ "Sung Kook Kim", "Arnaud J. Venet", "Aditya V. Thakur" ] }
    , { arxiv = "1909.05964", title = "Quantitative Programming by Examples", authors = [ "Sumit Gulwani", "Kunal Pathak", "Arjun Radhakrishna", "Ashish Tiwari", "Abhishek Udupa" ] }
    , { arxiv = "1909.05969", title = "A Note On Compliance Relations And Fixed Points", authors = [ "Maurizio Murgia" ] }
    , { arxiv = "1909.05970", title = "Rusty Variation: Deadlock-free Sessions with Failure in Rust", authors = [ "Wen Kokke" ] }
    , { arxiv = "1909.05971", title = "Towards Gradually Typed Capabilities in the Pi-Calculus", authors = [ "Matteo Cimini" ] }
    , { arxiv = "1909.06228", title = "IR2Vec: A Flow Analysis based Scalable Infrastructure for Program Encodings", authors = [ "Venkata Keerthy S", "Rohit Aggarwal", "Shalini Jain", "Maunendra Sankar Desarkar", "Ramakrishna Upadrasta", "Y. N. Srikant" ] }
    , { arxiv = "1909.06353", title = "That's C, baby. C!", authors = [ "Roberto Bagnara" ] }
    , { arxiv = "1909.07190", title = "Model-Based Warp-Level Tiling for Image Processing Programs on GPUs", authors = [ "Abhinav Jangda", "Arjun Guha" ] }
    , { arxiv = "1909.07331", title = "ReduKtor: How We Stopped Worrying About Bugs in Kotlin Compiler", authors = [ "Daniil Stepanov", "Marat Akhin", "Mikhail Belyaev" ] }
    , { arxiv = "1909.08230", title = "Prolog Coding Guidelines: Status and Tool Support", authors = [ "Falco Nogatz", "Philipp Körner", "Sebastian Krings" ] }
    , { arxiv = "1909.08557", title = "Default Disambiguation for Online Parsers", authors = [ "Lukas Diekmann", "Laurence Tratt" ] }
    , { arxiv = "1909.08671", title = "Mi-Cho-Coq, a framework for certifying Tezos Smart Contracts", authors = [ "Bruno Bernardo", "Raphaël Cauderlier", "Zhenlei Hu", "Basile Pesin", "Julien Tesson" ] }
    , { arxiv = "1909.08789", title = "Proof Pearl: Magic Wand as Frame", authors = [ "Qinxiang Cao", "Shengyi Wang", "Aquinas Hobor", "Andrew W. Appel" ] }
    , { arxiv = "1909.08815", title = "Supporting On-Stack Replacement in Unstructured Languages by Loop Reconstruction and Extraction", authors = [ "Raphael Mosaner", "David Leopoldseder", "Manuel Rigger", "Roland Schatz", "Hanspeter Mössenböck" ] }
    , { arxiv = "1909.08958", title = "On the Design, Implementation, and Use of Laziness in R", authors = [ "Aviral Goel", "Jan Vitek" ] }
    , { arxiv = "1909.09324", title = "Automated Verification of Integer Overflow", authors = [ "Asankhaya Sharma" ] }
    , { arxiv = "1909.09543", title = "Process Query Language: Design, Implementation, and Evaluation", authors = [ "Artem Polyvyanyy", "Arthur H. M. ter Hofstede", "Marcello La Rosa", "Chun Ouyang", "Anastasiia Pika" ] }
    , { arxiv = "1909.09562", title = "Equivalence Checking of Non-deterministic Operations", authors = [ "Sergio Antoy", "Michael Hanus" ] }
    , { arxiv = "1909.11206", title = "Using human-in-the-loop synthesis to author functional reactive programs", authors = [ "Julie L Newcomb", "Rastislav Bodik" ] }
    , { arxiv = "1909.12252", title = "Using E-Graphs for CAD Parameter Inference", authors = [ "Chandrakana Nandi", "Adam Anderson", "Max Willsey", "James R. Wilcox", "Eva Darulova", "Dan Grossman", "Zachary Tatlock" ] }
    , { arxiv = "1909.12279", title = "Fine-Grained, Language-Based Access Control for Database-Backed Applications", authors = [ "Ezra Zigmond", "Stephen Chong", "Christos Dimoulas", "Scott Moore" ] }
    , { arxiv = "1909.12795", title = "Automatically Tracing Imprecision Causes in JavaScript Static Analysis", authors = [ "Hongki Lee", "Changhee Park", "Sukyoung Ryu" ] }
    , { arxiv = "1909.13058", title = "Profiling minisat based on user defined execution time -- GPROF", authors = [ "Shubhendra Pal Singhal", "Sandeep Gupta", "Pierluigi Nuzzo" ] }
    , { arxiv = "1909.13649", title = "PlanAlyzer: Assessing Threats to the Validity of Online Experiments", authors = [ "Emma Tosch", "Eytan Bakshy", "Emery D. Berger", "David D. Jensen", "J. Eliot B. Moss" ] }
    , { arxiv = "1909.00973", title = "The Dynamics of Software Composition Analysis", authors = [ "Darius Foo", "Jason Yeo", "Hao Xiao", "Asankhaya Sharma" ] }
    , { arxiv = "1909.01640", title = "Defeating Opaque Predicates Statically through Machine Learning and Binary Analysis", authors = [ "Ramtine Tofighi-Shirazi", "Irina Asăvoae", "Philippe Elbaz-Vincent", "Thanh-Ha Le" ] }
    , { arxiv = "1909.01743", title = "Verifying the DPLL Algorithm in Dafny", authors = [ "Cezar-Constantin Andrici", "Ştefan Ciobâcă" ] }
    , { arxiv = "1909.01745", title = "Explaining SDN Failures via Axiomatisations", authors = [ "Georgiana Caltais" ] }
    , { arxiv = "1909.03110", title = "Making High-Performance Robots Safe and Easy to Use for an Introduction to Computing", authors = [ "Joseph Spitzer", "Joydeep Biswas", "Arjun Guha" ] }
    , { arxiv = "1909.03291", title = "Compiling PL/SQL Away", authors = [ "Christian Duta", "Denis Hirn", "Torsten Grust" ] }
    , { arxiv = "1909.06215", title = "Reasoning about call-by-value: a missing result in the history of Hoare's logic", authors = [ "Krzysztof R. Apt", "Frank S. de Boer" ] }
    , { arxiv = "1909.06344", title = "The Case for Writing Network Drivers in High-Level Programming Languages", authors = [ "Paul Emmerich", "Simon Ellmann", "Fabian Bonk", "Alex Egger", "Esaú García Sánchez-Torija", "Thomas Günzel", "Sebastian Di Luzio", "Alexandru Obada", "Maximilian Stadlmeier", "Sebastian Voit", "Georg Carle" ] }
    , { arxiv = "1909.07036", title = "Towards Distributed Logic Programming based on Computability Logic", authors = [ "Keehang Kwon" ] }
    , { arxiv = "1909.07479", title = "On correctness of an n queens program", authors = [ "Włodzimierz Drabent" ] }
    , { arxiv = "1909.07646", title = "Proceedings 35th International Conference on Logic Programming (Technical Communications)", authors = [ "Bart Bogaerts", "Esra Erdem", "Paul Fodor", "Andrea Formisano", "Giovambattista Ianni", "Daniela Inclezan", "German Vidal", "Alicia Villanueva", "Marina De Vos", "Fangkai Yang" ] }
    , { arxiv = "1909.07814", title = "CrypTFlow: Secure TensorFlow Inference", authors = [ "Nishant Kumar", "Mayank Rathee", "Nishanth Chandran", "Divya Gupta", "Aseem Rastogi", "Rahul Sharma" ] }
    , { arxiv = "1909.07918", title = "A Programming Framework for Differential Privacy with Accuracy Concentration Bounds", authors = [ "Elisabet Lobo-Vesga", "Alejandro Russo", "Marco Gaboardi" ] }
    , { arxiv = "1909.08232", title = "A Three-Valued Semantics for Typed Logic Programming", authors = [ "João Barbosa", "Mário Florido", "Vítor Santos Costa" ] }
    , { arxiv = "1909.08242", title = "Generating Local Search Neighborhood with Synthesized Logic Programs", authors = [ "Mateusz Ślażyński", "Salvador Abreu", "Grzegorz J. Nalepa" ] }
    , { arxiv = "1909.08243", title = "Quantified Constraint Handling Rules", authors = [ "Vincent Barichard", "Igor Stéphan" ] }
    , { arxiv = "1909.08246", title = "Extended Magic for Negation: Efficient Demand-Driven Evaluation of Stratified Datalog with Precise Complexity Guarantees", authors = [ "K. Tuncay Tekle", "Yanhong A. Liu" ] }
    , { arxiv = "1909.08248", title = "A Rule-Based System for Explainable Donor-Patient Matching in Liver Transplantation", authors = [ "Felicidad Aguado", "Pedro Cabalar", "Jorge Fandinno", "Brais Muñiz", "Gilberto Pérez", "Francisco Suárez" ] }
    , { arxiv = "1909.08261", title = "Research Report on Automatic Synthesis of Local Search Neighborhood Operators", authors = [ "Mateusz Ślażyński" ] }
    , { arxiv = "1909.09567", title = "Output-sensitive Information flow analysis", authors = [ "Cristian Ene", "Laurent Mounier", "Marie-Laure Potet" ] }
    , { arxiv = "1909.10493", title = "Formalism for Supporting the Development of Verifiably Safe Medical Guidelines with Statecharts", authors = [ "Chunhui Guo", "Zhicheng Fu", "Zhenyu Zhang", "Shangping Ren", "Lui Sha" ] }
    , { arxiv = "1909.12281", title = "Human-Centric Program Synthesis", authors = [ "Will Crichton" ] }
    , { arxiv = "1909.12582", title = "Towards Coq-verified Esterel Semantics and Compiling", authors = [ "Gérard Berry", "Lionel Rieg" ] }
    , { arxiv = "1909.13516", title = "Multi-Modal Attention Network Learning for Semantic Source Code Retrieval", authors = [ "Yao Wan", "Jingdong Shu", "Yulei Sui", "Guandong Xu", "Zhou Zhao", "Jian Wu", "Philip S. Yu" ] }
    , { arxiv = "1909.13639", title = "NeuroVectorizer: End-to-End Vectorization with Deep Reinforcement Learning", authors = [ "Ameer Haj-Ali", "Nesreen K. Ahmed", "Ted Willke", "Sophia Shao", "Krste Asanovic", "Ion Stoica" ] }
    , { arxiv = "1909.13768", title = "Backpropagation in the Simply Typed Lambda-calculus with Linear Negation", authors = [ "Alois Brunel", "Damiano Mazza", "Michele Pagani" ] }
    , { arxiv = "1910.00241", title = "Optimal Dyck Reachability for Data-Dependence and Alias Analysis", authors = [ "Krishnendu Chatterjee", "Bhavya Choudhary", "Andreas Pavlogiannis" ] }
    , { arxiv = "1910.00709", title = "Mμl: The Power of Dynamic Multi-Methods", authors = [ "Isaac Oscar Gariano", "Marco Servetto" ] }
    , { arxiv = "1910.02146", title = "RecordFlux: Formal Message Specification and Generation of Verifiable Binary Parsers", authors = [ "Tobias Reiher", "Alexander Senier", "Jeronimo Castrillon", "Thorsten Strufe" ] }
    , { arxiv = "1910.02375", title = "Design and Use of Loop-Transformation Pragmas", authors = [ "Michael Kruse", "Hal Finkel" ] }
    , { arxiv = "1910.03001", title = "Trading off Complexity for Expressiveness in Programming Languages: Visions and Preliminary Experiences", authors = [ "Vincenzo De Florio", "Chris Blondia" ] }
    , { arxiv = "1910.03784", title = "Generalized Property-Directed Reachability for Hybrid Systems", authors = [ "Kohei Suenaga", "Takuya Ishizawa" ] }
    , { arxiv = "1910.08480", title = "Gradual Typing for Extensibility by Rows", authors = [ "Taro Sekiyama", "Atsushi Igarashi" ] }
    , { arxiv = "1910.08607", title = "Exorcising Spectres with Secure Compilers", authors = [ "Marco Guarnieri", "Marco Patrignani" ] }
    , { arxiv = "1910.08634", title = "Universal Composability is Secure Compilation", authors = [ "Marco Patrignani", "Riad S. Wahby", "Robert Künneman" ] }
    , { arxiv = "1910.09521", title = "Reasoning About Recursive Tree Traversals", authors = [ "Yanjun Wang", "Jinwei Liu", "Dalin Zhang", "Xiaokang Qiu" ] }
    , { arxiv = "1910.09579", title = "Transparent Synchronous Dataflow", authors = [ "Steven W. T. Cheung", "Dan R. Ghica", "Koko Muroya" ] }
    , { arxiv = "1910.09586", title = "Memory Safety Preservation for WebAssembly", authors = [ "Marco Vassena", "Marco Patrignani" ] }
    , { arxiv = "1910.09744", title = "Decidable Synthesis of Programs with Uninterpreted Functions", authors = [ "Paul Krogmeier", "Umang Mathur", "Adithya Murali", "P. Madhusudan", "Mahesh Viswanathan" ] }
    , { arxiv = "1910.10421", title = "Towards a Complete Picture of Lens Laws", authors = [ "Keisuke Nakano" ] }
    , { arxiv = "1910.10889", title = "What's Decidable About Program Verification Modulo Axioms?", authors = [ "Umang Mathur", "P. Madhusudan", "Mahesh Viswanathan" ] }
    , { arxiv = "1910.10988", title = "A Polymorphic RPC Calculus", authors = [ "Kwanghoon Choi", "James Cheney", "Simon Fowler", "Sam Lindley" ] }
    , { arxiv = "1910.11108", title = "Model-View-Update-Communicate: Session Types meet the Elm Architecture", authors = [ "Simon Fowler" ] }
    , { arxiv = "1910.11629", title = "Runners in action", authors = [ "Danel Ahman", "Andrej Bauer" ] }
    , { arxiv = "1910.11714", title = "Pointer Life Cycle Types for Lock-Free Data Structures with Memory Reclamation", authors = [ "Roland Meyer", "Sebastian Wolff" ] }
    , { arxiv = "1910.11717", title = "Selective Lambda Lifting", authors = [ "Sebastian Graf", "Simon Peyton Jones" ] }
    , { arxiv = "1910.11724", title = "Embracing a mechanized formalization gap", authors = [ "Antal Spector-Zabusky", "Joachim Breitner", "Yao Li", "Stephanie Weirich" ] }
    , { arxiv = "1910.11741", title = "Implementing choreography extraction", authors = [ "Luís Cruz-Filipe", "Fabrizio Montesi", "Larisa Safina" ] }
    , { arxiv = "1910.11924", title = "A Calculus for Language Transformations", authors = [ "Benjamin Mourad", "Matteo Cimini" ] }
    , { arxiv = "1910.12256", title = "Complexity and Information in Invariant Inference", authors = [ "Yotam M. Y. Feldman", "Neil Immerman", "Mooly Sagiv", "Sharon Shoham" ] }
    , { arxiv = "1910.12272", title = "Declarative Semantics of the Hybrid Constraint Language HydLa", authors = [ "Kazunori Ueda", "Hiroshi Hosobe", "Daisuke Ishii" ] }
    , { arxiv = "1910.12935", title = "Precise Dataflow Analysis of Event-Driven Applications", authors = [ "Ming-Ho Yee", "Ayaz Badouraly", "Ondřej Lhoták", "Frank Tip", "Jan Vitek" ] }
    , { arxiv = "1910.14619", title = "Reductions for Safety Proofs (Extended Version)", authors = [ "Azadeh Farzan", "Anthony Vandikas" ] }
    , { arxiv = "1910.00577", title = "Structural Language Models of Code", authors = [ "Uri Alon", "Roy Sadaka", "Omer Levy", "Eran Yahav" ] }
    , { arxiv = "1910.00905", title = "Compositional Non-Interference for Fine-Grained Concurrent Programs", authors = [ "Dan Frumin", "Robbert Krebbers", "Lars Birkedal" ] }
    , { arxiv = "1910.01755", title = "Towards Constant-Time Foundations for the New Spectre Era", authors = [ "Sunjay Cauligi", "Craig Disselkoen", "Klaus v. Gleissenthall", "Dean Tullsen", "Deian Stefan", "Tamara Rezk", "Gilles Barthe" ] }
    , { arxiv = "1910.02874", title = "Field-based Coordination with the Share Operator", authors = [ "Giorgio Audrito", "Jacob Beal", "Ferruccio Damiani", "Danilo Pianini", "Mirko Viroli" ] }
    , { arxiv = "1910.03026", title = "IoTSim-Edge: A Simulation Framework for Modeling the Behaviour of IoT and Edge Computing Environments", authors = [ "Devki Nandan Jha", "Khaled Alwasel", "Areeb Alshoshan", "Xianghua Huang", "Ranesh Kumar Naha", "Sudheer Kumar Battula", "Saurabh Garg", "Deepak Puthal", "Philip James", "Albert Y. Zomaya", "Schahram Dustdar", "Rajiv Ranjan" ] }
    , { arxiv = "1910.03704", title = "Do People Prefer \"Natural\" code?", authors = [ "Casey Casalnuovo", "Kevin Lee", "Hulin Wang", "Prem Devanbu", "Emily Morgan" ] }
    , { arxiv = "1910.04137", title = "Automated Methods for Checking Differential Privacy", authors = [ "Gilles Barthe", "Rohit Chadha", "Vishal Jagannath", "A. Prasad Sistla", "Mahesh Viswanathan" ] }
    , { arxiv = "1910.05177", title = "Evaluating Semantic Representations of Source Code", authors = [ "Yaza Wainakh", "Moiz Rauf", "Michael Pradel" ] }
    , { arxiv = "1910.06500", title = "DeepVS: An Efficient and Generic Approach for Source Code Modeling Usage", authors = [ "Yasir Hussain", "Zhiqiu Huang", "Yu Zhou", "Senzhang Wang" ] }
    , { arxiv = "1910.06826", title = "Statically Detecting Vulnerabilities by Processing Programming Languages as Natural Languages", authors = [ "Ibéria Medeiros", "Nuno Neves", "Miguel Correia" ] }
    , { arxiv = "1910.07517", title = "Adversarial Examples for Models of Code", authors = [ "Noam Yefet", "Uri Alon", "Eran Yahav" ] }
    , { arxiv = "1910.07519", title = "On foundational aspects of RDF and SPARQL", authors = [ "Dominique Duval", "Rachid Echahed", "Frederic Prost" ] }
    , { arxiv = "1910.07583", title = "Abstract Transducers", authors = [ "Andreas Stahlbauer" ] }
    , { arxiv = "1910.08091", title = "MultiVerse: Causal Reasoning using Importance Sampling in Probabilistic Programming", authors = [ "Yura Perov", "Logan Graham", "Kostis Gourgoulias", "Jonathan G. Richens", "Ciarán M. Lee", "Adam Baker", "Saurabh Johri" ] }
    , { arxiv = "1910.08416", title = "Programming and Symbolic Computation in Maude", authors = [ "Francisco Durán", "Steven Eker", "Santiago Escobar", "Narciso Martí-Oliet", "José Meseguer", "Rubén Rubio", "Carolyn Talcott" ] }
    , { arxiv = "1910.09633", title = "Quantum Programming with Inductive Datatypes: Causality and Affine Type Theory", authors = [ "Romain Péchoux", "Simon Perdrix", "Mathys Rennela", "Vladimir Zamdzhiev" ] }
    , { arxiv = "1910.10346", title = "Knowledge of Uncertain Worlds: Programming with Logical Constraints", authors = [ "Yanhong A. Liu", "Scott D. Stoller" ] }
    , { arxiv = "1910.10851", title = "A Weakly Initial Algebra for Higher-Order Abstract Syntax in Cedille", authors = [ "Aaron Stump" ] }
    , { arxiv = "1910.11141", title = "Automatically Batching Control-Intensive Programs for Modern Accelerators", authors = [ "Alexey Radul", "Brian Patton", "Dougal Maclaurin", "Matthew D. Hoffman", "Rif A. Saurous" ] }
    , { arxiv = "1910.11232", title = "Overview of Logical Foundations of Cyber-Physical Systems", authors = [ "André Platzer" ] }
    , { arxiv = "1910.11471", title = "Machine Translation from Natural Language to Code using Long-Short Term Memory", authors = [ "K.M. Tahsin Hassan Rahit", "Rashidul Hasan Nabil", "Md Hasibul Huq" ] }
    , { arxiv = "1910.12643", title = "Ready, set, Go! Data-race detection and the Go language", authors = [ "Daniel Schnetzer Fava", "Martin Steffen" ] }
    , { arxiv = "1910.13346", title = "Intelligent-Unrolling: Exploiting Regular Patterns in Irregular Applications", authors = [ "Changxi Liu", "Hailong Yang", "Xu Liu", "Zhongzhi Luan", "Depei Qian" ] }
    , { arxiv = "1910.14560", title = "Data Abstraction and Relational Program Logic", authors = [ "Mohammad Nikouei", "Anindya Banerjee", "David A. Naumann" ] }
    , { arxiv = "1910.13324", title = "Divide, Conquer, and Combine: a New Inference Strategy for Probabilistic Programs with Stochastic Support", authors = [ "Yuan Zhou", "Hongseok Yang", "Yee Whye Teh", "Tom Rainforth" ] }
    , { arxiv = "1911.00268", title = "Modular Inference of Linear Types for Multiplicity-Annotated Arrows", authors = [ "Kazutaka Matsuda" ] }
    , { arxiv = "1911.00583", title = "Program Sketching with Live Bidirectional Evaluation", authors = [ "Justin Lubin", "Nick Collins", "Cyrus Omar", "Ravi Chugh" ] }
    , { arxiv = "1911.00705", title = "Label-Dependent Session Types", authors = [ "Peter Thiemann", "Vasco T. Vasconcelos" ] }
    , { arxiv = "1911.00815", title = "A Streaming Analytics Language for Processing Cyber Data", authors = [ "Eric L. Goodman", "Dirk Grunwald" ] }
    , { arxiv = "1911.03262", title = "ROSY: An elegant language to teach the pure reactive nature of robot programming", authors = [ "Hugo Pacheco", "Nuno Macedo" ] }
    , { arxiv = "1911.03807", title = "Synthesis of coordination programs from linear temporal logic", authors = [ "Suguman Bansal", "Kedar S. Namjoshi", "Yaniv Sa'ar" ] }
    , { arxiv = "1911.03926", title = "Gemini: A Functional Programming Language for Hardware Description", authors = [ "Aditya Srinivasan", "Andrew D. Hilton" ] }
    , { arxiv = "1911.04091", title = "Program Synthesis by Type-Guided Abstraction Refinement", authors = [ "Zheng Guo", "Michael James", "David Justo", "Jiaxiao Zhou", "Ziteng Wang", "Ranjit Jhala", "Nadia Polikarpova" ] }
    , { arxiv = "1911.04523", title = "A Simple Differentiable Programming Language", authors = [ "Martin Abadi", "Gordon D. Plotkin" ] }
    , { arxiv = "1911.04560", title = "Existential Types for Relaxed Noninterference", authors = [ "Raimil Cruz", "Éric Tanter" ] }
    , { arxiv = "1911.04588", title = "Recurrence Extraction for Functional Programs through Call-by-Push-Value (Extended Version)", authors = [ "G. A. Kavvos", "Edward Morehouse", "Daniel R. Licata", "Norman Danner" ] }
    , { arxiv = "1911.04631", title = "Scheme Macros for Non-linear Pattern Matching with Backtracking for Non-free Data Types", authors = [ "Satoshi Egi" ] }
    , { arxiv = "1911.06153", title = "Kind Inference for Datatypes: Technical Supplement", authors = [ "Ningning Xie", "Richard A. Eisenberg", "Bruno C. d. S. Oliveira" ] }
    , { arxiv = "1911.06391", title = "Reduction Monads and Their Signatures", authors = [ "Benedikt Ahrens", "André Hirschowitz", "Ambroise Lafont", "Marco Maggesi" ] }
    , { arxiv = "1911.06567", title = "Reconciling Event Structures with Modern Multiprocessors", authors = [ "Evgenii Moiseenko", "Anton Podkopaev", "Ori Lahav", "Orestis Melkonian", "Viktor Vafeiadis" ] }
    , { arxiv = "1911.07260", title = "Optimizing Ordered Graph Algorithms with GraphIt", authors = [ "Yunming Zhang", "Ajay Brahmakshatriya", "Xinyi Chen", "Laxman Dhulipala", "Shoaib Kamil", "Saman Amarasinghe", "Julian Shun" ] }
    , { arxiv = "1911.08033", title = "A Process Calculus for Formally Verifying Blockchain Consensus Protocols", authors = [ "Wolfgang Jeltsch" ] }
    , { arxiv = "1911.08286", title = "Zoea -- Composable Inductive Programming Without Limits", authors = [ "Edward McDaid", "Sarah McDaid" ] }
    , { arxiv = "1911.09668", title = "Visualization by Example", authors = [ "Chenglong Wang", "Yu Feng", "Rastislav Bodik", "Alvin Cheung", "Isil Dillig" ] }
    , { arxiv = "1911.10982", title = "A Foundation of Lazy Streaming Graphs", authors = [ "Philip Dexter", "Yu David Liu", "Kenneth Chiu" ] }
    , { arxiv = "1911.11376", title = "Mandala: A Smart Contract Programming Language", authors = [ "Markus Knecht" ] }
    , { arxiv = "1911.11824", title = "GOOL: A Generic Object-Oriented Language (extended version)", authors = [ "Jacques Carette", "Brooks MacLachlan", "W. Spencer Smith" ] }
    , { arxiv = "1911.12555", title = "Securing Smart Contract On The Fly", authors = [ "Ao Li", "Jemin Andrew Choi", "Fan Long" ] }
    , { arxiv = "1911.12557", title = "Expected Runtime of Quantum Programs", authors = [ "Junyi Liu", "Li Zhou", "Mingsheng Ying" ] }
    , { arxiv = "1911.12651", title = "Type Safety with JSON Subschema", authors = [ "Andrew Habib", "Avraham Shinnar", "Martin Hirzel", "Michael Pradel" ] }
    , { arxiv = "1911.12855", title = "Poq: Projection-based Runtime Assertions for Debugging on a Quantum Computer", authors = [ "Gushu Li", "Li Zhou", "Nengkun Yu", "Yufei Ding", "Mingsheng Ying", "Yuan Xie" ] }
    , { arxiv = "1911.12932", title = "Juniper: A Functional Reactive Programming Language for the Arduino", authors = [ "Caleb Helbling", "Samuel Z Guyer" ] }
    , { arxiv = "1911.00406", title = "Formalizing the Dependency Pair Criterion for Innermost Termination", authors = [ "Ariane Alves Almeida", "Mauricio Ayala-Rincon" ] }
    , { arxiv = "1911.00561", title = "Twin-Finder: Integrated Reasoning Engine for Pointer-related Code Clone Detection", authors = [ "Hongfa Xue", "Guru Venkataramani", "Tian Lan" ] }
    , { arxiv = "1911.01077", title = "Inferring Lower Runtime Bounds for Integer Programs", authors = [ "Florian Frohn", "Matthias Naaf", "Marc Brockschmidt", "Jürgen Giesl" ] }
    , { arxiv = "1911.02178", title = "A Language-based Serverless Function Accelerator", authors = [ "Emily Herbert", "Arjun Guha" ] }
    , { arxiv = "1911.02564", title = "Formality in Software Requirements", authors = [ "Jean-Michel Bruel", "Sophie Ebersold", "Florian Galinier", "Alexandr Naumchev", "Manuel Mazzara", "Bertrand Meyer" ] }
    , { arxiv = "1911.02624", title = "Data Generation for Neural Programming by Example", authors = [ "Judith Clymo", "Haik Manukian", "Nathanaël Fijalkow", "Adrià Gascón", "Brooks Paige" ] }
    , { arxiv = "1911.04026", title = "A generic imperative language for polynomial time", authors = [ "Daniel Leivant" ] }
    , { arxiv = "1911.04422", title = "Draw This Object: A Study of Debugging Representations", authors = [ "Matúš Sulír", "Ján Juhár" ] }
    , { arxiv = "1911.04710", title = "Aplib: Tactical Programming of Intelligent Agents", authors = [ "I. S. W. B. Prasetya" ] }
    , { arxiv = "1911.04732", title = "Smart Contract Interactions in Coq", authors = [ "Jakob Botsch Nielsen", "Bas Spitters" ] }
    , { arxiv = "1911.05660", title = "Enhancing Programmability, Portability, and Performance with Rich Cross-Layer Abstractions", authors = [ "Nandita Vijaykumar" ] }
    , { arxiv = "1911.05839", title = "Compile-time Parallelization of Subscripted Subscript Patterns", authors = [ "Akshay Bhosale", "Rudolf Eigenmann" ] }
    , { arxiv = "1911.05900", title = "Proceedings of the Third Workshop on Software Foundations for Data Interoperability (SFDI2019+), October 28, 2019, Fukuoka, Japan", authors = [ "Soichiro Hidaka", "Yasunori Ishihara", "Zachary G. Ives" ] }
    , { arxiv = "1911.07393", title = "Rebuttal to Berger et al., TOPLAS 2019", authors = [ "Baishakhi Ray", "Prem Devanbu", "Vladimir Filkov" ] }
    , { arxiv = "1911.07567", title = "What are the Actual Flaws in Important Smart Contracts (and How Can We Find Them)?", authors = [ "Alex Groce", "Josselin Feist", "Gustavo Grieco", "Michael Colburn" ] }
    , { arxiv = "1911.07707", title = "Building Fast Fuzzers", authors = [ "Rahul Gopinath", "Andreas Zeller" ] }
    , { arxiv = "1911.08174", title = "Failure of Normalization in Impredicative Type Theory with Proof-Irrelevant Propositional Equality", authors = [ "Andreas Abel", "Thierry Coquand" ] }
    , { arxiv = "1911.09421", title = "The Linear Algebra Mapping Problem", authors = [ "Christos Psarras", "Henrik Barthels", "Paolo Bientinesi" ] }
    , { arxiv = "1911.10081", title = "ptype: Probabilistic Type Inference", authors = [ "Taha Ceritli", "Christopher K. I. Williams", "James Geddes" ] }
    , { arxiv = "1911.10353", title = "Seamless Object-Oriented Requirements", authors = [ "Alexandr Naumchev" ] }
    , { arxiv = "1911.11184", title = "Managing Variability in Relational Databases by VDBMS", authors = [ "Parisa Ataei", "Qiaoran Li", "Eric Walkingshaw", "Arash Termehchy" ] }
    , { arxiv = "1911.11728", title = "OASIS: ILP-Guided Synthesis of Loop Invariants", authors = [ "Sahil Bhatia", "Saswat Padhi", "Nagarajan Natarajan", "Rahul Sharma", "Prateek Jain" ] }
    , { arxiv = "1911.11894", title = "FSE/CACM Rebuttal2: Correcting A Large-Scale Study of Programming Languages and Code Quality in GitHub", authors = [ "Emery D. Berger", "Petr Maj", "Olga Vitek", "Jan Vitek" ] }
    , { arxiv = "1911.02452", title = "XACC: A System-Level Software Infrastructure for Heterogeneous Quantum-Classical Computing", authors = [ "Alexander J. McCaskey", "Dmitry I. Lyakh", "Eugene F. Dumitrescu", "Sarah S. Powers", "Travis S. Humble" ] }
    , { arxiv = "1911.09755", title = "An Efficient Parametric Linear Programming Solver and Application to Polyhedral Projection", authors = [ "Hang Yu", "David Monniaux" ] }
    , { arxiv = "1912.00781", title = "Experiments with a PCCoder extension", authors = [ "Mircea-Dan Hernest" ] }
    , { arxiv = "1912.00981", title = "Proving Data-Poisoning Robustness in Decision Trees", authors = [ "Samuel Drews", "Aws Albarghouthi", "Loris D'Antoni" ] }
    , { arxiv = "1912.01289", title = "A Formal Approach to the Engineering of Domain-Specific Distributed Systems", authors = [ "Rocco De Nicola", "Gianluigi Ferrari", "Rosario Pugliese", "Francesco Tiezzi" ] }
    , { arxiv = "1912.02250", title = "A Verified Optimizer for Quantum Circuits", authors = [ "Kesha Hietala", "Robert Rand", "Shih-Han Hung", "Xiaodi Wu", "Michael Hicks" ] }
    , { arxiv = "1912.02499", title = "Perfectly Parallel Fairness Certification of Neural Networks", authors = [ "Caterina Urban", "Maria Christakis", "Valentin Wüstholz", "Fuyuan Zhang" ] }
    , { arxiv = "1912.02951", title = "User Experience with Language-Independent Formal Verification", authors = [ "Suhabe Bugrara" ] }
    , { arxiv = "1912.03584", title = "Formalizing Event-Driven Behavior of Serverless Applications", authors = [ "Matthew Obetz", "Stacy Patterson", "Ana Milanova" ] }
    , { arxiv = "1912.03854", title = "Variability-aware Datalog", authors = [ "Ramy Shahin", "Marsha Chechik" ] }
    , { arxiv = "1912.05036", title = "RVSDG: An Intermediate Representation for Optimizing Compilers", authors = [ "Nico Reissmann", "Jan Christian Meyer", "Helge Bahmann", "Magnus Själander" ] }
    , { arxiv = "1912.05234", title = "Array Languages Make Neural Networks Fast", authors = [ "Artjoms Šinkarovs", "Hans-Nikolai Vießmann", "Sven-Bodo Scholz" ] }
    , { arxiv = "1912.05601", title = "Practical Sized Typing for Coq", authors = [ "Jonathan Chan", "William J. Bowman" ] }
    , { arxiv = "1912.06791", title = "Approximations in Probabilistic Programs", authors = [ "Ekansh Sharma", "Daniel M. Roy" ] }
    , { arxiv = "1912.08255", title = "Decidable Tag-Based Semantic Subtyping for Nominal Types, Tuples, and Unions", authors = [ "Julia Belyakova" ] }
    , { arxiv = "1912.09770", title = "CacheQuery: Learning Replacement Policies from Hardware Caches", authors = [ "Pepe Vila", "Pierre Ganty", "Marco Guarnieri", "Boris Köpf" ] }
    , { arxiv = "1912.10135", title = "QuB: A Resource Aware Functional Programming Language", authors = [ "Apoorv Ingle" ] }
    , { arxiv = "1912.10630", title = "Deeply Integrating C11 Code Support into Isabelle/PIDE", authors = [ "Frédéric Tuong", "Burkhart Wolff" ] }
    , { arxiv = "1912.10631", title = "A Component-Based Formal Language Workbench", authors = [ "Peter D. Mosses" ] }
    , { arxiv = "1912.10817", title = "Using Prolog for Transforming XML-Documents", authors = [ "René Haberland" ] }
    , { arxiv = "1912.11281", title = "Aggressive Aggregation: a New Paradigm for Program Optimization", authors = [ "Frederik Gossen", "Marc Jasper", "Alnis Murtovi", "Bernhard Steffen" ] }
    , { arxiv = "1912.11929", title = "GASOL: Gas Analysis and Optimization for Ethereum Smart Contracts", authors = [ "Elvira Albert", "Jesús Correas", "Pablo Gordillo", "Guillermo Román-Díez", "Albert Rubio" ] }
    , { arxiv = "1912.12189", title = "LLOV: A Fast Static Data-Race Checker for OpenMP Programs", authors = [ "Utpal Bora", "Santanu Das", "Pankaj Kureja", "Saurabh Joshi", "Ramakrishna Upadrasta", "Sanjay Rajopadhye" ] }
    , { arxiv = "1912.12659", title = "Synthesizing Queries via Interactive Sketching", authors = [ "Osbert Bastani", "Xin Zhang", "Armando Solar-Lezama" ] }
    , { arxiv = "1912.13451", title = "Introduction to Rank-polymorphic Programming in Remora (Draft)", authors = [ "Olin Shivers", "Justin Slepak", "Panagiotis Manolios" ] }
    , { arxiv = "1912.00429", title = "PointEval: On the Impact of Pointer Analysis Frameworks", authors = [ "Jyoti Prakash", "Abhishek Tiwari", "Christian Hammer" ] }
    , { arxiv = "1912.00867", title = "A Probabilistic Approach to Floating-Point Arithmetic", authors = [ "Fredrik Dahlqvist", "Rocco Salvia", "George A Constantinides" ] }
    , { arxiv = "1912.01914", title = "A Quantitative Understanding of Pattern Matching", authors = [ "Sandra Alves", "Delia Kesner", "Daniel Ventura" ] }
    , { arxiv = "1912.02211", title = "A Constructive Formalization of the Weak Perfect Graph Theorem", authors = [ "Abhishek Kr Singh", "Raja Natarajan" ] }
    , { arxiv = "1912.04719", title = "Usability Methods for Designing Programming Languages for Software Engineers", authors = [ "Michael Coblenz", "Gauri Kambhatla", "Paulette Koronkevich", "Jenna L. Wise", "Celeste Barnaby", "Joshua Sunshine", "Jonathan Aldrich", "Brad A. Myers" ] }
    , { arxiv = "1912.05937", title = "Inferring Input Grammars from Dynamic Control Flow", authors = [ "Rahul Gopinath", "Björn Mathis", "Andreas Zeller" ] }
    , { arxiv = "1912.06878", title = "Conquering the Extensional Scalability Problem for Value-Flow Analysis Frameworks", authors = [ "Qingkai Shi", "Rongxin Wu", "Gang Fan", "Charles Zhang" ] }
    , { arxiv = "1912.09611", title = "Proceedings Fifth Workshop on Formal Integrated Development Environment", authors = [ "Rosemary Monahan", "Virgile Prevosto", "Jose Proença" ] }
    , { arxiv = "1912.09715", title = "A Paraconsistent ASP-like Language with Tractable Model Generation", authors = [ "Andrzej Szalas" ] }
    , { arxiv = "1912.10041", title = "Probabilistic process algebra and strategic interleaving", authors = [ "C. A. Middelburg" ] }
    , { arxiv = "1912.10629", title = "Automated Deductive Verification for Ladder Programming", authors = [ "Denis Cousineau", "David Mentré", "Hiroaki Inoue" ] }
    , { arxiv = "1912.11308", title = "ADD-Lib: Decision Diagrams in Practice", authors = [ "Frederik Gossen", "Alnis Murtovi", "Philip Zweihoff", "Bernhard Steffen" ] }
    , { arxiv = "1912.11951", title = "EVA: An Encrypted Vector Arithmetic Language and Compiler for Efficient Homomorphic Computation", authors = [ "Roshan Dathathri", "Blagovesta Kostova", "Olli Saarikivi", "Wei Dai", "Kim Laine", "Madanlal Musuvathi" ] }
    , { arxiv = "1912.12345", title = "Synthetic Datasets for Neural Program Synthesis", authors = [ "Richard Shin", "Neel Kant", "Kavi Gupta", "Christopher Bender", "Brandon Trabucco", "Rishabh Singh", "Dawn Song" ] }
    , { arxiv = "1912.12700", title = "On the Performance and Energy Efficiency of the PGAS Programming Model on Multicore Architectures", authors = [ "Jérémie Lagravière", "Johannes Langguth", "Mohammed Sourouri", "Phuong H. Ha", "Xing Cai" ] }
    , { arxiv = "1912.13122", title = "Towards Regulated Deep Learning", authors = [ "Andrés García-Camino" ] }
    , { arxiv = "1912.13477", title = "Interaction laws of monads and comonads", authors = [ "Shin-ya Katsumata", "Exequiel Rivas", "Tarmo Uustalu" ] }
    , { arxiv = "1912.11554", title = "Composable Effects for Flexible and Accelerated Probabilistic Programming in NumPyro", authors = [ "Du Phan", "Neeraj Pradhan", "Martin Jankowiak" ] }
    ]
