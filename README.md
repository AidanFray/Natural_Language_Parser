# Natural_Lanuage_Parser
University AI assignment that translates Natural language (english) into a UNIX command

The agent takes in english language, parses it using contex free grammer (rules I'll place below) and returns the command that performs that action on the UNIX console.

For example:
  
	a very short command listing the current directory
  
Will return:
  
	ls
  
The agent also has synonyms support, so the agent can understand:
  
	a very short command listing the current folder
  
And will return
  
	ls
	
This extra understanding it without implementing another rule.

# Grammer Rules
• S -> NP VP

• S -> VP


• NP -> Det NP2

• NP -> NP2

• NP -> NP PP


• NP2 -> Noun

• NP2 -> Adj NP2


• PP -> Prep NP


• VP -> Verb

• VP -> Verb Adverb NP

• VP -> Verb Adverb

• VP -> Verb PP

• VP -> Verb NP

**Where:**

S = Sentence

NP = Noun Phrase

VP = Verb Phrase

Det = Determinate

PP = Prepositional Phrase

Adj = Adjective

NP2 = 2nd Noun Phrase


These rules can be used to break down sentences to grab important parts or check if the entred sentences are valid.
