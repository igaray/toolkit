#!/usr/bin/python
import random
import os

verse01 = """
  1 ䷀ The Creative

     The Judgement

          The Creative works sublime success,
          Furthering through perseverance.

     The Image

          The movement of heaven is full of power.
          Thus the superior man makes himself strong and untiring.

     The Lines

          Nine at the beginning means:
          Hidden dragon. Do not act.

          Nine in the second place means:
          Dragon appearing in the field.
          It furthers one to see the great man.

          Nine in the third place means:
          All day long the superior man is creatively active.
          At nightfall his mind is still beset with cares.
          Danger. No blame.

          Nine in the fourth place means:
          Wavering flight over the depths.
          No blame.

     ()   Nine in the fifth place means:
          Flying dragon in the heavens.
          It furthers one to see the great man.

          Nine at the top means:
          Arrogant dragon will have cause to repent.

          When all the lines are nines, it means:
          There appears a flight of dragons without heads.
          Good fortune.
"""
verse02 = """
  2 ䷁ The Receptive

     The Judgement

          The Receptive brings about sublime success,
          Furthering through the perseverance of a mare.
          If the superior man undertakes something and tries to lead,
          He goes astray;
          But if he follows, he finds guidance.
          It is favorable to find friends in the west and south,
          To forego friends in the east and north.
          Quiet perseverance brings good fortune.

     The Image

          The earth's condition is receptive devotion.
          Thus the superior man who has breadth of character
          Carries the outer world.

     The Lines

          Six at the beginning means:
          When there is hoarfrost underfoot,
          Solid ice is not far off.

     ()   Six in the second place means:
          Straight, square, great.
          Without purpose,
          Yet nothing remains unfurthered.

          Six in the third place means:
          Hidden lines.
          One is able to remain persevering.
          If by chance you are in the service of a king,
          Seek not works, but bring to completion.

          Six in the fourth place means:
          A tied-up sack. No blame, no praise.

          Six in the fifth place means:
          A yellow lower garment brings supreme good fortune.

          Six at the top means:
          Dragons fight in the meadow.
          Their blood is black and yellow.

          When all the lines are sixes, it means:
          Lasting perseverance furthers.
"""
verse03 = """
  3 ䷂ Difficulty at the Beginning

     The Judgement

          Difficulty at the Beginning works supreme success,
          Furthering through perseverance.
          Nothing should be undertaken.
          It furthers one to appoint helpers.

     The Image

          Clouds and thunder:
          The image of Difficulty at the Beginning.
          Thus the superior man
          Brings order out of confusion.

     The Lines

     ()   Nine at the beginning means:
          Hesitation and hindrance.
          It furthers one to remain persevering.
          It furthers one to appoint helpers.

          Six in the second place means:
          Difficulties pile up.
          Horse and wagon part.
          He is not a robber;
          He wants to woo when the time comes.
          The maiden is chaste,
          She does not pledge herself.
          Ten years-then she pledges herself.

          Six in the third place means:
          Whoever hunts deer without the forester
          Only loses his way in the forest.
          The superior man understands the signs of the time
          And prefers to desist.
          To go on brings humiliation.

          Six in the fourth place means:
          Horse and wagon part.
          Strive for union.
          To go brings g    A little perseverance brings good fortune.
          Great perseverance brings misfortune.

          Six at the top means:
          Horse and wagon part.
          Bloody tears flow.
"""
verse04 = """
  4 ䷃ Youthful Folly

     The Judgement

          Youthful Folly has success.
          It is not I who seek the young fool;
          The young fool seeks me.
          At the first oracle I inform him.
          If he asks two or three times, it is importunity.
          If he importunes, I give him no information.
          Perseverance furthers.

     The Image

          A spring wells up at the foot of the mountain:
          The image of Youth.
          Thus the superior man fosters his character
          By thoroughness in all that he does.

     The Lines

          Six at the beginning means:
          To make a fool develop
          It furthers one to apply discipline.
          The fetters should be removed.
          To go on in this way brings humiliation.

     ()   Nine in the second place means:
          To bear with fools in kindliness brings good fortune.
          To know how to take women
          Brings supreme good fortune.
          The son is capable of taking charge of the household.

          Six in the third place means:
          Take not a maiden who, when she sees a man of bronze,
          Loses possession of herself.
          Nothing furthers.

          Six in the fourth place means:
          Entangled folly brings humiliation.

     ()   Six in the fifth place means:
          Childlike folly brings good fortune.

          Nine at the top means:
          In punishing folly
          It does not further one
          To commit transgressions.
          The only thing that furthers
          Is to prevent transgressions.
"""
verse05 = """
  5 ䷄ Waiting (Nourishment)

     The Judgement

          Waiting. If you are sincere,
          You have light and success.
          Perseverance brings good fortune.
          It furthers one to cross the great water.

     The Image

          Clouds rise up to heaven:
          The image of Waiting.
          Thus the superior man eats and drinks,
          Is joyous and of good cheer.

     The Lines

          Nine at the beginning means:
          Waiting in the meadow.
          It furthers one to abide in what endures.
          No blame.

          Nine in the second place means:
          Waiting on the sand.
          There is some gossip.
          The end brings good fortune.

          Nine in the third place means:
          Waiting in the mud
          Brings about the arrival of the enemy.

          Six in the fourth place means:
          Waiting in blood.
          Get out of the pit.

     ()   Nine in the fifth place means:
          Waiting at meat and drink.
          Perseverance brings good fortune.

          Six at the top means:
          One falls into the pit.
          Three uninvited guests arrive.
          Honor them, and in the end there will be good fortune.
"""
verse06 = """
  6 ䷅ Sung / Conflict

     The Judgement

          Conflict. You are sincere
          And are being obstructed.
          A cautious halt halfway brings good fortune.
          Going through to the end brings misfortune.
          It furthers one to see the great man.
          It does not further one to cross the great water.

     The Image

          Heaven and water go their opposite ways:
          The image of Conflict.
          Thus in all his transactions the superior man
          Carefully considers the beginning.

     The Lines

          Six at the beginning means:
          If one does not perpetuate the affair,
          There is a little gossip.
          In the end, good fortune comes.

          Nine in the second place means:
          One cannot engage in conflict;
          One returns home, gives way.
          The people induces perseverance.
          Danger. In the end, good fortune comes.
          If by chance you are in the service of a king,
          Seek not works.

          Nine in the fourth place means:
          One cannot engage in conflict.
          One turns back and submits to fate,
          Changes one's attitude,
          And finds peace in perseverance.
          Good fortune.

     ()   Nine in the fifth place means:
          To contend before him
          Brings good fortune.

          Nine at the top means:
          Even if by chance a leather belt is bestowed on one,
          By the end of a morning
          It will have been snatched away three times.
"""
verse07 = """
  7 ䷆ The Army

     The Judgement

          The Army. The army needs perseverance
          And a strong man.
          Good fortune without blame.

     The Image

          In the middle of the earth is water:
          The image of the Army.
          Thus the superior man increases his masses
          By generosity toward the people.

     The Lines

          Six at the beginning means:
          An army must set forth in proper order.
          If the order is not good, misfortune threatens.

     ()   Nine in the second place means:
          In the midst of the army.
          Good fortune. No blame.
          The king bestows a triple decoration.

          Six in the third place means:
          Perchance the army carries corpses in the wagon.
          Misfortune.

          Six in the fourth place means:
          The army retreats. No blame.

     ()   Six in the fifth place means:
          There is game in the field.
          It furthers one to catch it.
          Without blame.
          Let the eldest lead the army.
          The younger transports corpses;
          Then perseverance brings misfortune.

          Six at the top means:
          The great prince issues commands,
          Founds states, vests families with fiefs.
          Inferior people should not be employed.
"""
verse08 = """
  8 ䷇ Holding Together [Union]

     The Judgement

          Holding Together brings good fortune.
          Inquire of the oracle once again
          Whether you possess sublimity, constancy, and perseverance;
          Then there is no blame.
          Those who are uncertain gradually join.
          Whoever comes too late
          Meets with misfortune.

     The Image

          On the earth is water:
          The image of Holding Together.
          Thus the kings of antiquity
          Bestowed the different states as fiefs
          And cultivated friendly relations
          With the feudal lords.

     The Lines

          Six at the beginning means:
          Hold to him in truth and loyalty;
          This is without blame.
          Truth, like a full earthen bowl:
          Thus in the end
          Good fortune comes from without.

          Six in the second place means:
          Hold to him inwardly.
          Perseverance brings good fortune.

          Six in the third place means:
          You hold together with the wrong people.

          Six in the fourth place means:
          Hold to him outwardly also.
          Perseverance brings good fortune.

     ()   Nine in the fifth place means:
          Manifestation of holding together.
          In the hunt the king uses beaters on three sides only
          And foregoes game that runs off in front.
          The citizens need no warning.
          Good fortune.
"""
verse09 = """
  9 ䷈ The Taming Power of the Small

     The Judgement

          The Taming Power of the Small
          Has success.
          Dense clouds, no rain from our western region.

     The Image

          The wind drives across heaven:
          The image of The Taming Power of the Small.
          Thus the superior man
          Refines the outward aspect of his nature.

     The Lines

          Nine at the beginning means:
          Return to the way.
          How could there be blame in this?
          Good fortune.

          Nine in the second place means:
          He allows himself to be drawn into returning.
          Good fortune.

          Nine in the third place means:
          The spokes burst out of the wagon wheels.
          Man and wife roll their eyes.

     []   Six in the fourth place means:
          If you are sincere, blood vanishes and fear gives way.
          No blame.

     ()   Nine in the fifth place means:
          If you are sincere and loyally attached,
          You are rich in your neighbor.

          Nine at the top means:
          The rain comes, there is rest.
          This is due to the lasting effect of character.
          Perseverance brings the woman into danger.
          The moon is nearly full.
          If the superior man persists,
          Misfortune comes.
"""
verse10 = """
  10 ䷉ Treading [Conduct]

     The Judgement

          Treading. Treading upon the tail of the tiger.
          It does not bite the man. Success.

     The Image

          Heaven above, the lake below:
          The image of Treading.
          Thus the superior man discriminates between high and low,
          And thereby fortifies the thinking of the people.

     The Lines

          Nine at the beginning means:
          Simple conduct. Progress without blame.

          Nine in the second place means:
          Treading a smooth, level course.
          The perseverance of a dark man
          Brings good fortune.

     []   Six in the third place means:
          A one-eyed man is able to see,
          A lame man is able to tread.
          He treads on the tail of the tiger.
          The tiger bites the man.
          Misfortune.
          Thus does a warrior act on behalf of his great prince.

          Nine in the fourth place means:
          He treads on the tail of the tiger.
          Caution and circumspection
          Lead ultimately to good fortune.

     ()   Nine in the fifth place means:
          Resolute conduct.
          Perseverance with awareness of danger.

          Nine at the top means:
          Look to your conduct and weigh the favorable signs.
          When everything is fulfilled, supreme good fortune comes.
"""
verse11 = """
  11 ䷊ Peace

     The Judgement

          Peace. The small departs,
          The great approaches.
          Good fortune. Success.

     The Image

          Heaven and earth unite: the image of Peace.
          Thus the ruler
          Divides and completes the course of heaven and earth;
          He furthers and regulates the gifts of heaven and earth,
          And so aids the people.

     The Lines

          Nine at the beginning means:
          When ribbon grass is pulled up, the sod comes with it.
          Each according to his kind.
          Undertakings bring good fortune.

     ()   Nine in the second place means:
          Bearing with the uncultured in gentleness,
          Fording the river with resolution,
    slope.
          No going not followed by a return.
          He who remains persevering in danger
          Is without blame.
          Do not complain about this truth;
          Enjoy the good fortune you still possess.

          Six in the fourth place means:
          He flutters down, not boasting of his wealth,
          Together with his neighbor,
          Guileless and sincere.

     ()   Six in the fifth place means:
          The sovereign I
          Gives his daughter in marriage.
          This brings blessing
          And supreme good fortune.

          Six at the top means:
          The wall falls back into the moat.
          Use no army now.
          Make your commands known within your own town.
          Perseverance brings humiliation.
"""
verse12 = """
  12 ䷋ Standstill [Stagnation]

     The Judgement

          Standstill. Evil people do not further
          The perseverance of the superior man.
          The great departs; the small approaches.

     The Image

          Heaven and earth do not unite:
          The image of Standstill.
          Thus the superior man falls back upon his inner worth
          In order to escape the difficulties.
          He does not permit himself to be honored with revenue.

     The Lines

          Six at the beginning means:
          When ribbon grass is pulled up, the sod comes with it.
          Each according to his kind.
          Perseverance brings good fortune and success.

     []   Six in the second place means:
          They bear and endure;
          This means good fortune for inferior people.
          The standstill serves to help the great man to attain success.

          Six in the third place means:
          They bear shame.

          Nine in the fourth place means:
          He who acts at the command of the highest
          Remains without blame.
          Those of like mind partake of the blessing.

     ()   Nine in the fifth place means:
          Standstill is giving way.
          Good fortune for the great man.
          "What if it should fail, what if it should fail?"
          In this way he ties it to a cluster of mulberry shoots.

          Nine at the top means:
          The standstill comes to an end.
          First standstill, then good fortune.
"""
verse13 = """
  13 ䷌ Fellowship with Men

     The Judgement

          Fellowship with Men in the open.
          Success.
          It furthers one to cross the great water.
          The perseverance of the superior man furthers.

     The Image

          Heaven together with fire:
          The image of Fellowship with Men.
          Thus the superior man organizes the clans
          And makes distinctions between things.

     The Lines

          Nine at the beginning means:
          Fellowship with men at the gate.
          No blame.

     ()   Six in the second place means:
          Fellowship with men in the clan.
          Humiliation.

          Nine in the third place means:
          He hides weapons in the thicket;
          He climbs the high hill in front of it.
          For three years he does not rise up.

          Nine in the fourth place means:
          He climbs up on his wall; he cannot attack.
          Good fortune.

     ()   Nine in the fifth place means:
          Men bound in fellowship first weep and lament,
          But afterward they laugh.
          After great struggles they succeed in meeting.

          Nine at the top means:
          Fellowship with men in the meadow.
          No remorse.
"""
verse14 = """
  14 ䷍ Possession in Great Measure

     The Judgement

          Possession in Great Measure.
          Supreme success.

     The Image

          Fire in heaven above:
          The image of Possession in Great Measure.
          Thus the superior man curbs evil and furthers good,
          And thereby obeys the benevolent will of heaven.

     The Lines

          Nine at the beginning means:
          No relationship with what is harmful;
          There is no blame in this.
          If one remains conscious of difficulty,
          One remains without blame.

          Nine in the second place means:
          A big wagon for loading.
          One may undertake something.
          No blame.

          Nine in the third place means:
          A prince offers it to the Son of Heaven.
          A petty man cannot do this.

          Nine in the fourth place means:
          He makes a difference
          Between himself and his neighbor.
          No blame.

     ()   Six in the fifth place means:
          He whose truth is accessible, yet dignified,
          Has good fortune.

          Nine at the top means:
          He is blessed by heaven.
          Good fortune.
          Nothing that does not further.
"""
verse15 = """
  15 ䷎ Modesty

     The Judgement

          Modesty creates success.
          The superior man carries things through.

     The Image

          Within the earth, a mountain:
          The image of Modesty.
          Thus the superior man reduces that which is too much,
          And augments that which is too little.
          He weighs things and makes them equal.

     The Lines

          Six at the beginning means:
          A superior man modest about his modesty
          May cross the great water.
          Good fortune.

          Six in the second place means:
          Modesty that comes to expression.
          Perseverance brings good fortune.

     ()   Nine in the third place means:
          A superior man of modesty and merit
          Carries things to conclusion.
          Good fortune.

          Six in the fourth place means:
          Nothing that would not further modesty
          In movement.

          Six in the fifth place means:
          No boasting of wealth before one's neighbor.
          It is favorable to attack with force.
          Nothing that would not further.

          Six at the top means:
          Modesty that comes to expression.
          It is favorable to set armies marching
          To chastise one's own city and one's country.
"""
verse16 = """
  16 ䷏ Enthusiasm

     The Judgement

          Enthusiasm. It furthers one to install helpers
          And to set armies marching.

     The Image

          Thunder comes resounding out of the earth:
          The image of Enthusiasm.
          Thus the ancient kings made music
          In order to honor merit,
          And offered it with splendor
          To the Supreme Deity,
          Inviting their ancestors to be present.

     The Lines

          Six at the beginning means:
          Enthusiasm that expresses itself
          Brings misfortune.

          Six in the second place means:
          Firm as a rock. Not a whole day.
          Perseverance brings good fortune.

          Six in the third place means:
          Enthusiasm that looks upward creates remorse.
          Hesitation brings remorse.

     ()   Nine in the fourth place means:
          The source of enthusiasm.
          He achieves great things.
          Doubt not.
          You gather friends around you
          As a hair clasp gathers the hair.
          Six at the top means:
          Deluded enthusiasm.
          But if after completion one changes,
          There is no blame.
"""
verse17 = """
  17 ䷐ Following

     The Judgement

          Following has supreme success.
          Perseverance furthers. No blame.

     The Image

          Thunder in the middle of the lake:
          The image of Following.
          Thus the superior man at nightfall
          Goes indoors for rest and recuperation.

     The Lines

     ()   Nine at the beginning means:
          The standard is changing.
          Perseverance brings good fortune.
          To go out of the door in company
          Produces deeds.

          Six in the second place means:
          If one clings to the little boy,
          One loses the strong man.

          Six in the third place means:
          If one clings to the strong man,
          One loses the little boy.
          Through following one finds what one seeks.
          It furthers one to remain persevering.

          Nine in the fourth place means:
          Following creates success.
          Perseverance brings misfortune.
          To go one's way with sincerity brings clarity.
          How could there be blame in this?

     ()   Nine in the fifth place means:
          Sincere in the good. Good fortune.

          Six at the top means:
          He meets with firm allegiance
          And is still further bound.
          The king introduces him
          To the Western Mountain.
"""
verse18 = """
  18 ䷑ Work on What Has Been Spoiled [Decay]

     The Judgement

          Work on What Has Been Spoiled
          Has supreme success.
          It furthers one to cross the great water.
          Before the starting point, three days.
          After the starting point, three days.

     The Image

          The wind blows low on the mountain:
          The image of Decay.
          Thus the superior man stirs up the people
          And strengthens their spirit.

     The Lines

          Six at the beginning means:
          Setting right what has been spoiled by the father.
          If there is a son,
          No blame rests upon the departed father.
          Danger. In the end good fortune.

          Nine in the second place means:
          Setting right what has been spoiled by the mother.
          One must not be too persevering.

          Nine in the third place means:
          Setting right what has been spoiled by the father.
          There will be little remorse. No great blame.

          Six in the fourth place means:
          Tolerating what has been spoiled by the father.
          In continuing one sees humiliation.

     ()   Six in the fifth place means:
          Setting right what has been spoiled by the father.
          One meets with praise.

          Nine at the top means:
          He does not serve kings and princes,
          Sets himself higher goals.
"""
verse19 = """
  19 ䷒ Approach

     The Judgement

          Approach has supreme success.
          Perseverance furthers.
          When the eighth month comes,
          There will be misfortune.

     The Image

          The earth above the lake:
          The image of Approach.
          Thus the superior man is inexhaustible
          In his will to teach,
          And without limits
          In his tolerance and protection of Joint approach.
          Good fortune.
          Everything furthers.

          Six in the third place means:
          Comfortable approach.
          Nothing that would further.
          If one is induced to grieve over it,
          One becomes free of blame.

          Six in the fourth place means:
          Complete approach.
          No blame.

          Six in the fifth place means:
          Wise approach.
          This is right for a great prince.
          Good fortune.

          Six at the top means:
          Greathearted approach.
          Good fortune. No blame.
"""
verse20 = """
  20 ䷓ Contemplation (View)

     The Judgement

          Contemplation. The ablution has been made,
          But not yet the offering.
          Full of trust they look up to him.

     The Image

          The wind blows over the earth:
          The image of Contemplation.
          Thus the kings of old visited the regions of the world,
          Contemplated the people,
          And gave them instruction.

     The Lines

          Six at the beginning means:
          Boylike contemplation.
          For an inferior man, no blame.
          For a superior man, humiliation.

          Six in the second place means:
          Contemplation through the crack of the door.
          Furthering for the perseverance of a woman.

          Six in the third place means:
          Contemplation of my life
          Decides the choice
          Between advance and retreat.

          Six in the fourth place means:
          Contemplation of the light of the kingdom.
          It furthers one to exert influence as the guest of a king.

     ()   Nine in the fifth place means:
          Contemplation of my life.
          The superior man is without blame.

     ()   Nine at the top means:
          Contemplation of his life.
          The superior man is without blame.
"""
verse21 = """
  21 ䷔ Biting Through

     The Judgement

          Biting Through has success.
          It is favorable to let justice be administered.

     The Image

          Thunder and lightning:
          The image of Biting Through.
          Thus the kings of former times made firm the laws
          Through clearly defined penalties.

     The Lines

          Nine at the beginning means:
          His feet are fastened in the stocks,
          So that his toes disappear.
          No blame.

          Six in the second place means:
          Bites through tender meat,
          So that his nose disappears.
          No blame.

          Six in the third place means:
          Bites on old dried meat
          And strikes on something poisonous.
          Slight humiliation. No blame.

          Nine in the fourth place means:
          Bites on dried gristly meat.
          Receives metal arrows.
          It furthers one to be mindful of difficulties
          And to be persevering.
          Good fortune.

     ()   Six in the fifth place means:
          Bites on dried lean meat.
          Receives yellow gold.
          Perseveringly aware of danger.
          No blame.

          Nine at the top means:
          His neck is fastened in the wooden cangue,
          So that his ears disappear.
          Misfortune.
"""
verse22 = """
  22 ䷕ Grace

     The Judgement

          Grace has success.
          In small matters
          It is flearing up current affairs.
          But he dare not decide controversial issues in this way.

     The Lines

          Nine at the beginning means:
          He lends grace to his toes, leaves the carriage, and walks.

     ()   Six in the second place means:
          Lends grace to the beard on his chin.

          Nine in the third place means:
          Graceful and moist.
          Constant perseverance brings good fortune.

          Six in the fourth place means:
          Grace or simplicity?
          A white horse comes as if on wings.
          He is not a robber,
          He will woo at the right time.

          Six in the fifth place means:
          Grace in hills and gardens.
          The roll of silk is meager and small.
          Humiliation, but in the end good fortune.

     ()   Nine at the top means:
          Simple grace. No blame.
"""
verse23 = """
  23 ䷖ Splitting Apart

     The Judgement

          Splitting Apart. It does not further one
          To go anywhere.

     The Image

          The mountain rests on the earth:
          The image of Splitting Apart.
          Thus those above can ensure their position
          Only by giving generously to those below.

     The Lines

          Six at the beginning means:
          The leg of the bed is split.
          Those who persevere are destroyed.
          Misfortune.

          Six in the second place means:
          The bed is split at the edge.
          Those who persevere are destroyed.
          Misfortune.

          Six in the third place means:
          He splits with them. No blame.

          Six in the fourth place means:
          The bed is split up to the skin.
          Misfortune.

          Six in the fifth place means:
          A shoal of fishes. Favor comes through the court ladies.
          Everything acts to further.

     ()   Nine at the top means:
          There is a large fruit still uneaten.
          The superior man receives a carriage.
          The house of the inferior man is split apart.
"""
verse24 = """
  24 ䷗ Return (The Turning Point)

     The Judgement

          Return. Success.
          Going out and coming in without error.
          Friends come without blame.
          To and fro goes the way.
          On the seventh day comes return.
          It furthers one to have somewhere to go.

     The Image

          Thunder within the earth:
          The image of the Turning Point.
          Thus the kings of antiquity closed the passes
          At the time of solstice.
          Merchants and strangers did not go about,
          And the ruler
          Did not travel through the provinces.

     The Lines

     ()   Nine at the beginning means:
          Return from a short distance.
          No need for remorse.
          Great good fortune.

          Six in the second place means:
          Quiet return. Good fortune.

          Six in the third place means:
          Repeated return. Danger. No blame.

          Six in the fourth place means:
          Walking in the midst of others,
          One returns alone.

          Six in the fifth place means:
          Noblehearted return.  No remorse.

          Six at the top means:
          Missing the return. Misfortune.
          Misfortune from within and without.
          If armies are set marching in this way,
          One will in the end suffer a great defeat,
          Disastrous for the ruler of the country.
          For ten years
          It will not be possible to attack again.
"""
verse25 = """
  25 ䷘ Innocence (The Unexpected)

     The Judgement

          Innocence. Supreme success.
          Perseverance furthers.
          If someone is not as he should be,
          He has misfortune,
          And it does not further him
          To undertake anything.

     The Image

          Under heaven thunder rolls:
          All things attain the natural state of innocence.
          Thus the kings of old,
          Rich in virtue, and in harmony with the time,
          Fostered and nourished all beings.

     The Lines

     ()   Nine at the beginning means:
          Innocent behavior brings good fortune.

          Six in the second place means:
          If one does not count on the harvest while plowing,
          Nor on the use of the ground while clearing it,
          It       The cow that was tethered by someone
          Is the wanderer's gain, the citizen's loss.

          Nine in the fourth place means:
          He who can be persevering
          Remains without blame.

     ()   Nine in the fifth place means:
          Use no medicine in an illness
          Incurred through no fault of your own.
          It will pass of itself.

          Nine at the top means:
          Innocent action brings misfortune.
          Nothing furthers.
"""
verse26 = """
  26 ䷙ The Taming Power of the Great

     The Judgement

          The Taming Power of the Great.
          Perseverance furthers.
          Not eating at home brings good fortune.
          It furthers one to cross the great water.

     The Image

          Heaven within the mountain:
          The image of the Taming Power of the Great.
          Thus the superior man acquaints himself with many sayings of antiquity
          And many deeds of the past,
          In order to strengthen his character thereby.

     The Lines

          Nine at the beginning means:
          Danger is at hand. It furthers one to desist.

          Nine in the second place means:
          The axletrees are taken from the wagon.

          Nine in the third place means:
          A good horse that follows others.
          Awareness of danger,
          With perseverance, furthers.
          Practice chariot driving and armed defense daily.
          It furthers one to have somewhere to go.

          Six in the fourth place means:
          The headboard of a young bull.
          Great good fortune.

     ()   Six in the fifth place means:
          The tusk of a gelded boar.
          Good fortune.

     ()   Nine at the top means:
          One attains the way of heaven. Success.
"""
verse27 = """
  27 ䷚ The Corners of the Mouth (Providing Nourishment)

     The Judgement

          The Corners of the Mouth.
          Perseverance brings good fortune.
          Pay heed to the providing of nourishment
          And to what a man seeks
          To fill his own mouth with.

     The Image

          At the foot of the mountain, thunder:
          The image of Providing Nourishment.
          Thus the superior man is careful of his words
          And temperate in eating and drinking.

     The Lines

          Nine at the beginning means:
          You let your magic tortoise go,
          And look at me with the corners of your mouth drooping.
          Misfortune.

          Six in the second place means:
          Turning to the summit for nourishment,
          Deviating from the path
          To seek nourishment from the hill.
          Continuing to do this brings misfortune.

          Six in the third place means:
          Turning away from nourishment.
          Perseverance brings misfortune.
          Do not act thus for ten years.
          Nothing serves to further.

          Six in the fourth place means:
          Turning to the summit
          For provision of nourishment
          Brings good fortune.
          Spying about with sharp eyes
          Like a tiger with insatiable craving.
          No blame.

     ()   Six in the fifth place means:
          Turning away from the path.
          To remain persevering brings good fortune.
          One should not cross the great water.

     ()   Nine at the top means:
          The source of nourishment.
          Awareness of danger brings good fortune.
          It furthers one to cross the great water.
"""
verse28 = """
  28 ䷛

     The Judgement

          Preponderance of the Great.
          The ridgepole sags to the breaking point.
          It furthers one to have somewhere to go.
          Success.

     The Image

          The lake rises above the trees:
          The image of Preponderance of the Great.
          Thus the superior man, when he stands alone,
          Is unconcerned,
          And if he has to renounce the world,
          He is undaunted.

     The Lines

          Six at the beginning means:
          To spread white rushes underneath.
          No blame.

     ()   Nine in the second place means:
          A dry poplar sprouts at the root.
          An older man takes a young wife.
          Everything furthers.

          Nine in the third place means:
          The ridgepole sags to the breaking point.
          Misfortune.

     ()   Nine in the fourth place means:
          The ridgepole is braced. Good fortune.
          If there are ulterior motives, it is humiliating.

          Nine in the fifth place means:
          A withered poplar puts forth flowers.
          An older woman takes a husband.
          No blame. No praise.

          Six at the top means:
          One must go through the water.
          It goes over one's head.
          Misfortune. No blame.
"""
verse29 = """
  29 ䷜ The Abysmal (Water)

     The Judgement

          The Abysmal repeated.
          If you are sincere, you have success in your heart,
          And whatever you do succeeds.

     The Image

          Water flows on uninterruptedly and reaches its goal:
          The image of the Abysmal repeated.
          Thus the superior man walks in lasting virtue
          And carries on the business of teaching.

     The Lines

          Six at the beginning means:
          Repetition of the Abysmal.
          In the abyss one falls into a pit.
          Misfortune.

     ()   Nine in the second place means:
          The abyss is dangerous.
          One should strive to attain small things only.

          Six in the third place means:
          Forward and backward, abyss on abyss.
          In danger like this, pause at first and wait,
          Otherwise you will fall into a pit in the abyss.
          Do not act in this way.

          Six in the fourth place means:
          A jug of wine, a bowl of rice with it;
          Earthen vessels
          Simply handed in through the window.
          There is certainly no blame in this.

     ()   Nine in the fifth place means:
          The abyss is not filled to overflowing,
          It is filled only to the rim.
          No blame.

          Six at the top means:
          Bound with cords and ropes,
          Shut in between thorn-hedged prison walls:
          For three years one does not find the way.
          Misfortune.
"""
verse30 = """
  30 ䷝ The Clinging, Fire

     The Judgement

          The Clinging. Perseverance furthers.
          It brings success.
          Care of the cow brings good fortune.

     The Image

          That which is bright rises twice:
          The image of Fire.
          Thus the great man, by perpetuating this brightness,
          Illumines the four quarters of the world.

     The Lines

          Nine at the beginning means:
          The footprints run crisscross.
          If one is seriously intent, no blame.

     ()   Six in the second place means:
          Yellow light. Supreme good fortune.

          Nine in the third place means:
          In the light of the setting sun,
     flames up, dies down, is thrown away.

     ()   Six in the fifth place means:
          Tears in floods, sighing and lamenting.
          Good fortune.

          Nine at the top means:
          The king uses him to march forth and chastise.
          Then it is best to kill the leaders
          And take captive the followers. No blame.
"""
verse31 = """
  31 ䷞ Influence (Wooing)

     The Judgement

          Influence. Success.
          Perseverance furthers.
          To take a maiden to wife brings good fortune.

     The Image

          A lake on the mountain:
          The image of Influence.
          Thus the superior man encourages people to approach him
          By his readiness to receive them.

     The Lines

          Six at the beginning means:
          The influence shows itself in the big toe.

          Six in the second place means:
          The influence shows itself in the calves of the legs.
          Misfortune.
          Tarrying brings good fortune.

          Nine in the third place means:
          The influence shows itself in the thighs.
          Holds to that which follows it.
          To continue is humiliating.

     ()   Nine in the fourth place means:
          Perseverance brings good fortune.
          Remorse disappears.
          If a man is agitated in mind,
          And his thoughts go hither and thither,
          Only those friends
          On whom he fixes his conscious thoughts
          Will follow.

     ()   Nine in the fifth place means:
          The influence shows itself in the back of the neck.
          No remorse.

          Six at the top means:
          The influence shows itself in the jaws, cheeks, and tongue.
"""
verse32 = """
  32 ䷟ Duration

     The Judgement

          Duration. Success. No blame.
          Perseverance furthers.
          It furthers one to have somewhere to go.

     The Image

          Thunder and wind: the image of Duration.
          Thus the superior man stands firm
          And does not change his direction.

     The Lines

          Six at the beginning means:
          Seeking duration too hastily brings misfortune persistently.
          Nothing that would further.

     ()   Nine in the second place means:
          Remorse disappears.

          Nine in the third place means:
          He who does not give duration to his character
          Meets with disgrace.
          Persistent humiliation.

          Nine in the fourth place means:
          No game in the field.

          Six in the fifth place means:
          Giving duration to one's character through perseverance.
          This is good fortune for a woman, misfortune for a man.

          Six at the top means:
          Restlessness as an enduring condition brings misfortune.
"""
verse33 = """
  33 ䷠ Retreat

     The Judgement

          Retreat. Success.
          In what is small, perseverance furthers.

     The Image

          Mountain under heaven: the image of Retreat.
          Thus the superior man keeps the inferior man at a distance,
          Not angrily but with reserve.

     The Lines

     []   Six at the beginning means:
          At the tail in retreat. This is dangerous.
          One must not wish to undertake anything.

     []   Six in the second place means:
          He holds him fast with yellow oxhide.
          No one can tear him
          Brings good fortune.

          Nine in the fourth place means:
          Voluntary retreat brings good fortune to the superior man
          And downfall to the inferior man.

     ()   Nine in the fifth place means:
          Friendly retreat. Perseverance brings good fortune.

          Nine at the top means:
          Cheerful retreat. Everything serves to further.
"""
verse34 = """
  34 ䷡ The Power of the Great

     The Judgement

          The Power of the Great. Perseverance furthers.

     The Image

          Thunder in heaven above:
          The image of the Power of the Great.
          Thus the superior man does not tread upon paths
          That do not accord with established order.

     The Lines

          Nine at the beginning means:
          Power in the toes.
          Continuing brings misfortune.
          This is certainly true.

          Nine in the second place means:
          Perseverance brings good fortune.

          Nine in the third place means:
          The inferior man works through power.
          The superior man does not act thus.
          To continue is dangerous.
          A goat butts against a hedge
          And gets its horns entangled.

     ()   Nine in the fourth place means:
          Perseverance brings good fortune.
          Remorse disappears.
          The hedge opens; there is no entanglement.
          Power depends upon the axle of a big cart.

          Six in the fifth place means:
          Loses the goat with ease.
          No remorse.

          Six at the top means:
          A goat butts against a hedge.
          It cannot go backward, it cannot go forward.
          Nothing serves to further.
          If one notes the difficulty, this brings good fortune.
"""
verse35 = """
  35 ䷢ Progress

     The Judgement

          Progress. The powerful prince
          Is honored with horses in large numbers.
          In a single day he is granted audience three times.

     The Image

          The sun rises over the earth:
          The image of Progress.
          Thus the superior man himself
          Brightens his bright virtue.

     The Lines

          Six at the beginning means:
          Progressing, but turned back.
          Perseverance brings good fortune.
          If one meets with no confidence, one should remain calm.
          No mistake.

          Six in the second place means:
          Progressing, but in sorrow.
          Perseverance brings good fortune.
          Then one obtains great happiness from one's ancestress.

          Six in the third place means:
          All are in accord. Remorse disappears.

          Nine in the fourth place means:
          Progress like a hamster.
          Perseverance brings danger.

     ()   Six in the fifth place means:
          Remorse disappears.
          Take not gain and loss to heart.
          Undertakings bring good fortune.
          Everything serves to further.

          Nine at the top means:
          Making progress with the horns is permissible
          Only for the purpose of punishing one's own city.
          To be conscious of danger brings good fortune.
          No blame.
          Perseverance brings humiliation.
"""
verse36 = """
  36 ䷣ Darkening of the Light

     The Judgement

          Darkening of the Light. In adversity
          It furthers one to be persegreat mass:
          He veils his light, yet still shines.

     The Lines

          Nine at the beginning means:
          Darkening of the light during flight.
          He lowers his wings.
          The superior man does not eat for three days
          On his wanderings.
          But he has somewhere to go.
          The host has occasion to gossip about him.

     ()   Six in the second place means:
          Darkening of the light injures him in the left thigh.
          He gives aid with the strength of a horse.
          Good fortune.

          Nine in the third place means:
          Darkening of the light during the hunt in the south.
          Their great leader is captured.
          One must not expect perseverance too soon.

          Six in the fourth place means:
          He penetrates the left side of the belly.
          One gets at the very heart of the darkening of the light,
          And leaves gate and courtyard.

     ()   Six in the fifth place means:
          Darkening of the light as with Prince Chi.
          Perseverance furthers.

     []   Six at the top means:
          Not light but darkness.
          First he climbed up to heaven,
          Then he plunged into the depths of the earth.
"""
verse37 = """
  37 ䷤ The Family [The Clan]

     The Judgement

          The Family. The perseverance of the woman furthers.

     The Image

          Wind comes forth from fire:
          The image of the Family.
          Thus the superior man has substance in his words
          And duration in his way of life.

     The Lines

          Nine at the beginning means:
          Firm seclusion within the family.
          Remorse disappears.

     ()   Six in the second place means:
          She should not follow her whims.
          She must attend within to the food.
          Perseverance brings good fortune.

          Nine in the third place means:
          When tempers flare up in the family,
          Too great severity brings remorse.
          Good fortune nonetheless.
          When woman and child dally and laugh,
          It leads in the end to humiliation.

          Six in the fourth place means:
          She is the treasure of the house.
          Great good fortune.

     ()   Nine in the fifth place means:
          As a king he approaches his family.
          Fear not.
          Good fortune.

          Nine at the top means:
          His work commands respect.
          In the end good fortune comes.
"""
verse38 = """
  38 ䷥ Opposition

     The Judgement

          Opposition. In small matters, good fortune.

     The Image

          Above, fire; below, the lake:
          The image of Opposition.
          Thus amid all fellowship
          The superior man retains his individuality.

     The Lines

          Nine at the beginning means:
          Remorse disappears.
          If you lose your horse, do not run after it;
          It will come back of its own accord.
          When you see evil people,
          Guard yourself against mistakes.

     ()   Nine in the second place means:
          One meets his lord in a narrow street.
          No blame.

          Six in the third place means:
          One sees the wagon dragged back,
          The oxen halted,
          A man's hair and nose cut off.
          Not a good beginning, but a good end.

          Nine in the fourth place means:
          Isolated through opposition,
          One meets a like-minded man
          With whom one can associat         How could it be a mistake?

          Nine at the top means:
          Isolated through opposition,
          One sees one's companion as a pig covered with dirt,
          As a wagon full of devils.
          First one draws a bow against him,
          Then one lays the bow aside.
          He is not a robber; he will woo at the right time.
          As one goes, rain falls; then good fortune comes.
"""
verse39 = """
  39 ䷦ Obstruction

     The Judgement

          Obstruction. The southwest furthers.
          The northeast does not further.
          It furthers one to see the great man.
          Perseverance brings good fortune.

     The Image

          Water on the mountain:
          The image of Obstruction.
          Thus the superior man turns his attention to himself
          And molds his character.

     The Lines

          Six at the beginning means:
          Going leads to obstructions,
          Coming meets with praise.

          Six in the second place means:
          The king's servant is beset by obstruction upon obstruction,
          But it is not his own fault.

          Nine in the third place means:
          Going leads to obstructions;
          Hence he comes back.

          Six in the fourth place means:
          Going leads to obstructions,
          Coming leads to union.

     ()   Nine in the fifth place means:
          In the midst of the greatest obstructions,
          Friends come.

          Six at the top means:
          Going leads to obstructions,
          Coming leads to great good fortune.
          It furthers one to see the great man.
"""
verse40 = """
  40 ䷧ Deliverance

     The Judgement

          Deliverance. The southwest furthers.
          If there is no longer anything where one has to go,
          Return brings good fortune.
          If there is still something where one has to go,
          Hastening brings good fortune.

     The Image

          Thunder and rain set in:
          The image of Deliverance.
          Thus the superior man pardons mistakes
          And forgives misdeeds.

     The Lines

          Six at the beginning means:
          Without blame.

     ()   Nine in the second place means:
          One kills three foxes in the field
          And receives a yellow arrow.
          Perseverance brings good fortune.

          Six in the third place means:
          If a man carries a burden on his back
          And nonetheless rides in a carriage,
          He thereby encourages robbers to draw near.
          Perseverance leads to humiliation.

          Nine in the fourth place means:
          Deliver yourself from your great toe.
          Then the companion comes,
          And him you can trust.

     ()   Six in the fifth place means:
          If only the superior man can deliver himself,
          It brings good fortune.
          Thus he proves to inferior men that he is in earnest.

          Six at the top means:
          The prince shoots at a hawk on a high wall.
          He kills it. Everything serves to further.
"""
verse41 = """
  41 ䷨ Decrease

     The Judgement

          Decrease combined with sincerity
          Brings about supreme good fortune
          Without blame.
          One may be persevering in this.
          It furthers one to undertake something.
          How is this e superior man controls his anger
          And restrains his instincts.

     The Lines

          Nine at the beginning means:
          Going quickly when one's tasks are finished
          Is without blame.
          But one must reflect on how much one may decrease others.

          Nine in the second place means:
          Perseverance furthers.
          To undertake something brings misfortune.
          Without decreasing oneself,
          One is able to bring increase to others.

     []   Six in the third place means:
          When three people journey together,
          Their number decreases by one.
          When one man journeys alone,
          He finds a companion.

          Six in the fourth place means:
          If a man decreases his faults,
          It makes the other hasten to come and rejoice.
          No blame.

     ()   Six in the fifth place means:
          Someone does indeed increase him.
          Ten pairs of tortoises cannot oppose it.
          Supreme good fortune.

     []   Nine at the top means:
          If one is increased without depriving others,
          There is no blame.
          Perseverance brings good fortune.
          It furthers one to undertake something.
          One obtains servants
          But no longer has a separate home.
"""
verse42 = """
  42 ䷩ Increase

     The Judgement

          Increase. It furthers one
          To undertake something.
          It furthers one to cross the great water.

     The Image

          Wind and thunder: the image of Increase.
          Thus the superior man:
          If he sees good, he imitates it;
          If he has faults, he rids himself of them.

     The Lines

     []   Nine at the beginning means:
          It furthers one to accomplish great deeds.
          Supreme good fortune. No blame.

     ()   Six in the second place means:
          Someone does indeed increase him;
          Ten pairs of tortoises cannot oppose it.
          Constant perseverance brings good fortune.
          The king presents him before God.
          Good fortune.

          Six in the third place means:
          One is enriched through unfortunate events.
          No blame, if you are sincere
          And walk in the middle,
          And report with a seal to the prince.

     []   Six in the fourth place means:
          If you walk in the middle
          And report to the prince,
          He will follow.
          It furthers one to be used
          In the removal of the capital.

     ()   Nine in the fifth place means:
          If in truth you have a kind heart, ask not.
          Supreme good fortune.
          Truly, kindness will be recognized as your virtue.

          Nine at the top means:
          He brings increase to no one.
          Indeed, someone even strikes him.
          He does not keep his heart constantly steady.
          Misfortune.
"""
verse43 = """
  43 ䷪ Break-through (Resoluteness)

     The Judgement

          Break-through. One must resolutely make the matter known
          At the court of the king.
          It must be announced truthfully. Danger.
          It is necessary to notify one's own city.
          It does not further to resort to arms.
          It furthers one to undertake something.

     The Image

          The lake has risen up to heaven:
          The image of Break-through.
          Thus the superior man
          Dispenses riches downward
          And refrains from resting on his virtue.

     The Lines

          Nine at the beginning means:
  A cry of alarm. Arms at evening and at night.
          Fear nothing.

          Nine in the third place means:
          To be powerful in the cheekbones
          Brings misfortune.
          The superior man is firmly resolved.
          He walks alone and is caught in the rain.
          He is bespattered,
          And people murmur against him.
          No blame.

          Nine in the fourth place means:
          There is no skin on his thighs,
          And walking comes hard.
          If a man were to let himself be led like a sheep,
          Remorse would disappear.
          But if these words are heard
          They will not be believed.

     ()   Nine in the fifth place means:
          In dealing with weeds,
          Firm resolution is necessary.
          Walking in the middle
          Remains free of blame.

     []   Six at the top means:
          No cry.
          In the end misfortune comes.
"""
verse44 = """
  44 ䷫ Coming to Meet

     The Judgement

          Coming to Meet. The maiden is powerful.
          One should not marry such a maiden.

     The Image

          Under heaven, wind:
          The image of Coming to Meet.
          Thus does the prince act when disseminating his commands
          And proclaiming them to the four quarters of heaven.

     The Lines

     []   Six at the beginning means:
          It must be checked with a brake of bronze.
          Perseverance brings good fortune.
          If one lets it take its course, one experiences misfortune.
          Even a lean pig has it in him to rage around.

     ()   Nine in the second place means:
          There is a fish in the tank. No blame.
          Does not further guests.

          Nine in the third place means:
          There is no skin on his thighs,
          And walking comes hard.
          If one is mindful of the danger,
          No great mistake is made.

          Nine in the fourth place means:
          No fish in the tank.
          This leads to misfortune.

     ()   Nine in the fifth place means:
          A melon covered with willow leaves.
          Hidden lines.
          Then it drops down to one from heaven.

          Nine at the top means:
          He comes to meet with his horns.
          Humiliation. No blame.
"""
verse45 = """
  45 ䷬ Gathering Together [Massing]

     The Judgement

          Gathering Together. Success.
          The king approaches his temple.
          It furthers one to see the great man.
          This brings success. Perseverance furthers.
          To bring great offerings creates good fortune.
          It furthers one to undertake something.

     The Image

          Over the earth, the lake:
          The image of Gathering Together.
          Thus the superior man renews his weapons
          In order to meet the unforseen.

     The Lines

          Six at the beginning means:
          If you are sincere, but not to the end,
          There will sometimes be confusion, sometimes gathering together.
          If you call out,
          Then after one grasp of the hand you can laugh again.
          Regret not. Going is without blame.

          Six in the second place means:
          Letting oneself be drawn
          Brings good fortune and remains blameless.
          If one is sincere,
          It furthers one to bring even a small offering.

          Six in the third place means:
          Gathering together amid sighs.
          Nothing that would further.
          Going is without blame.
          Slight humiliation.

     ()   Nine in the fourth place means:
          Great good fortune. No blame.

     ()   Nine in the fifth This brings no blame.
          If there are some who are not yet sincerely in the work,
          Sublime and enduring perseverance is needed.
          Then remorse disappears.

          Six at the top means:
          Lamenting and sighing, floods of tears.
          No blame.
"""
verse46 = """
  46 ䷭ Pushing Upward

     The Judgement

          Pushing Upward has supreme success.
          One must see the great man.
          Fear not.
          Departure toward the south
          Brings good fortune.

     The Image

          Within the earth, wood grows:
          The image of Pushing Upward.
          Thus the superior man of devoted character
          Heaps up small things
          In order to achieve something high and great.

     The Lines

     []   Six at the beginning means:
          Pushing upward that meets with confidence
          Brings great good fortune.

          Nine in the second place means:
          If one is sincere,
          It furthers one to bring even a small offering.
          No blame.

          Nine in the third place means:
          One pushes upward into an empty city.

          Six in the fourth place means:
          The king offers him Mount Ch'i.
          Good fortune. No blame.

     ()   Six in the fifth place means:
          Perseverance brings good fortune.
          One pushes upward by steps.

          Six at the top means:
          Pushing upward in darkness.
          It furthers one
          To be unremittingly persevering.
"""
verse47 = """
  47 ䷮ Oppression (Exhaustion)

     The Judgement

          Oppression. Success. Perseverance.
          The great man brings about good fortune.
          No blame.
          When one has something to say,
          It is not believed.

     The Image

          There is no water in the lake:
          The image of Exhaustion.
          Thus the superior man stakes his life
          On following his will.

     The Lines

          Six at the beginning means:
          One sits oppressed under a bare tree
          And strays into a gloomy valley.
          For three years one sees nothing.

     ()   Nine in the second place means:
          One is oppressed while at meat and drink.
          The man with the scarlet knee bands is just coming.
          It furthers one to offer sacrifice.
          To set forth brings misfortune.
          No blame.

          Six in the third place means:
          A man permits himself to be oppressed by stone,
          And leans on thorns and thistles.
          He enters his house and does not see his wife.
          Misfortune.

          Nine in the fourth place means:
          He comes very quietly, oppressed in a golden carriage.
          Humiliation, but the end is reached.

     ()   Nine in the fifth place means:
          His nose and feet are cut off.
          Oppression at the hands of the man with the purple knee bands.
          Joy comes softly.
          It furthers one to make offerings and libations.

          Six at the top means:
          He is oppressed by creeping vines.
          He moves uncertainly and says, "Movement brings remorse."
          If one feels remorse over this and makes a start,
          Good fortune comes.
"""
verse48 = """
  48 ䷯ The Well

     The Judgement

          The Well. The town may be changed,
          But the well cannot be changed.
          It neither decreases nor incrgo all the way,
          Or the jug breaks, it brings misfortune.

     The Image

          Water over wood: the image of the Well.
          Thus the superior man encourages the people at their work,
          And exhorts them to help one another.

     The Lines

          Six at the beginning means:
          One does not drink the mud of the well.
          No animals come to an old well.

          Nine in the second place means:
          At the wellhole one shoots fishes.
          The jug is broken and leaks.

          Nine in the third place means:
          The well is cleaned, but no one drinks from it.
          This is my heart's sorrow,
          For one might draw from it.
          If the king were clear-minded,
          Good fortune might be enjoyed in common.

          Six in the fourth place means:
          The well is being lined. No blame.

     ()   Nine in the fifth place means:
          In the well there is a clear, cold spring
          From which one can drink.

          Six at the top means:
          One draws from the well
          Without hindrance.
          It is dependable.
          Supreme good fortune.
"""
verse49 = """
  49 ䷰ Revolution (Molting)

     The Judgement

          Revolution. On your own day
          You are believed.
          Supreme success,
          Furthering through perseverance.
          Remorse disappears.

     The Image

          Fire in the lake: the image of Revolution.
          Thus the superior man
          Sets the calendar in order
          And makes the seasons clear.

     The Lines

          Nine at the beginning means:
          Wrapped in the hide of a yellow cow.

          Six in the second place means:
          When one's own day comes, one may create revolution.
          Starting brings good fortune. No blame.

          Nine in the third place means:
          Starting brings misfortune.
          Perseverance brings danger.
          When talk of revolution has gone the rounds three times,
          One may commit himself,
          And men will believe him.

          Nine in the fourth place means:
          Remorse disappears. Men believe him.
          Changing the form of government brings good fortune.

     ()   Nine in the fifth place means:
          The great man changes like a tiger.
          Even before he questions the oracle
          He is believed.

          Six at the top means:
          The superior man changes like a panther.
          The inferior man molts in the face.
          Starting brings misfortune.
          To remain persevering brings good fortune.
"""
verse50 = """
  50 ䷱ The Caldron

     The Judgement

          The Caldron. Supreme good fortune.
          Success.

     The Image

          Fire over wood:
          The image of the Caldron.
          Thus the superior man consolidates his fate
          By making his position correct.

     The Lines

          Six at the beginning means:
          A _ t_ i_ n_ g with legs upturned.
          Furthers removal of stagnating stuff.
          One takes a concubine for the sake of her son.
          No blame.

          Nine in the second place means:
          There is food in the _ t_ i_ n_ g.
          My comrades are envious,
          But they cannot harm me.
          Good fortune.

          Nine in the third place means:
          The handle of the _ t_ i_ n_ g is altered.
          One is impeded in his way of life.
          The fat of the pheasant is not erson is soiled.
          Misfortune.

     ()   Six in the fifth place means:
          The _ t_ i_ n_ g has yellow handles, golden carrying rings.
          Perseverance furthers.

     ()   Nine at the top means:
          The _ t_ i_ n_ g has rings of jade.
          Great good fortune.
          Nothing that would not act to further.
"""
verse51 = """
  51 ䷲ The Arousing (Shock, Thunder)

     The Judgement

          Shock brings success.
          Shock comes-oh, oh!
          Laughing words-ha, ha!
          The shock terrifies for a hundred miles,
          And he does not let fall the sacrificial spoon and chalice.

     The Image

          Thunder repeated: the image of Shock.
          Thus in fear and trembling
          The superior man sets his life in order
          And examines himself.

     The Lines

     ()   Nine at the beginning means:
          Shock comes-oh, oh!
          Then follow laughing words-ha, ha!
          Good fortune.

          Six in the second place means:
          Shock comes bringing danger.
          A hundred thousand times
          You lose your treasures
          And must climb the nine hills.
          Do not go in pursuit of them.
          After seven days you will get them back.

          Six in the third place means:
          Shock comes and makes one distraught.
          If shock spurs to action
          One remains free of misfortune.

          Nine in the fourth place means:
          Shock is mired.

          Six in the fifth place means:
          Shock goes hither and thither.
          Danger.
          However, nothing at all is lost.
          Yet there are things to be done.

          Six at the top means:
          Shock brings ruin and terrified gazing around.
          Going ahead brings misfortune.
          If it has not yet touched one's own body
          But has reached one's neighbor first,
          There is no blame.
          One's comrades have something to talk about.
"""
verse52 = """
  52 ䷳ Keeping Still, Mountain

     The Judgement

          Keeping Still. Keeping his back still
          So that he no longer feels his body.
          He goes into his courtyard
          And does not see his people.
          No blame.

     The Image

          Mountains standing close together:
          The image of Keeping Still.
          Thus the superior man
          Does not permit his thoughts
          To go beyond his situation.

     The Lines

          Six at the beginning means:
          Keeping his toes still.
          No blame.
          Continued perseverance furthers.

          Six in the second place means:
          Keeping his calves still.
          He cannot rescue him whom he follows.
          His heart is not glad.

          Nine in the third place means:
          Keeping his hips still.
          Making his sacrum stiff.
          Dangerous. The heart suffocates.

          Six in the fourth place means:
          Keeping his trunk still.
          No blame.

          Six in the fifth place means:
          Keeping his jaws still.
          The words have order.
          Remorse disappears.

     ()   Nine at the top means:
          Noblehearted keeping still.
          Good fortune.
"""
verse53 = """
  53 ䷴ Development (Gradual Progress)

     The Judgement

          Development.
          Thus the superior man abides in dignity and virtue,
          In order to improve the mores.

     The Lines

          Six at the beginning means:
          The wild goose gradually draws near the shore.
          The young son is in danger.
          There is talk. No blame.

     ()   Six in the second place means:
          The wild goose gradually draws near the cliff.
          Eating and drinking in peace and concord.
          Good fortune.

          Nine in the third place means:
          The wild goose gradually draws near the plateau.
          The man goes forth and does not return.
          The woman carries a child but does not bring it forth.
          Misfortune.
          It furthers one to fight off robbers.

          Six in the fourth place means:
          The wild goose gradually draws near the tree.
          Perhaps it will find a flat branch. No blame.

     ()   Nine in the fifth place means:
          The wild goose gradually draws near the summit.
          For three years the woman has no child.
          In the end nothing can hinder her.
          Good fortune.

          Nine at the top means:
          The wild goose gradually draws near the cloud heights.
          Its feathers can be used for the sacred dance.
          Good fortune.
"""
verse54 = """
  54 ䷵ The Marrying Maiden

     The Judgement

          The Marrying Maiden.
          Undertakings bring misfortune.
          Nothing that would further.

     The Image

          Thunder over the lake:
          The image of the Marrying Maiden.
          Thus the superior man
          Understands the transitory
          In the light of the eternity of the end.

     The Lines

          Nine at the beginning means:
          The marrying maiden as a concubine.
          A lame man who is able to tread.
          Undertakings bring good fortune.

          Nine in the second place means:
          A one-eyed man who is able to see.
          The perseverance of a solitary man furthers.

     []   Six in the third place means:
          The marrying maiden as a slave.
          She marries as a concubine.

          Nine in the fourth place means:
          The marrying maiden draws out the allotted time.
          A late marriage comes in due course.

     ()   Six in the fifth place means:
          The sovereign I gave his daughter in marriage.
          The embroidered garments of the princess
          Were not as gorgeous
          As those of the servingmaid.
          The moon that is nearly full
          Brings good fortune.

     []   Six at the top means:
          The woman holds the basket, but there are no fruits in it.
          The man stabs the sheep, but no blood flows.
          Nothing that acts to further.
"""
verse55 = """
  55 ䷶ Abundance [Fullness]

     The Judgement

          Abundance has success.
          The king attains abundance.
          Be not sad.
          Be like the sun at midday.

     The Image

          Both thunder and lightning come:
          The image of Abundance.
          Thus the superior man decides lawsuits
          And carries out punishments.

     The Lines

          Nine at the beginning means:
          When a man meets his destined ruler,
          They can be together ten days,
          And it is not a mistake.
          Going meets with recognition.

          Six in the second place means:
          The curtain is of such fullness
          That the polestars can be seen at noon.
          Through going one meets wi   That the small stars can be seen at noon.
          He breaks his right arm. No blame.

          Nine in the fourth place means:
          The curtain is of such fullness
          That the polestars can be seen at noon.
          He meets his ruler, who is of like kind.
          Good fortune.

     ()   Six in the fifth place means:
          Lines are coming,
          Blessing and fame draw near.
          Good fortune.

          Six at the top means:
          His house is in a state of abundance.
          He screens off his family.
          He peers through the gate
          And no longer perceives anyone.
          For three years he sees nothing.
          Misfortune.
"""
verse56 = """
  56 ䷷ The Wanderer

     The Judgement

          The Wanderer. Success through smallness.
          Perseverance brings good fortune
          To the wanderer.

     The Image

          Fire on the mountain:
          The image of the Wanderer.
          Thus the superior man
          Is clear-minded and cautious
          In imposing penalties,
          And protracts no lawsuits.

     The Lines

          Six at the beginning means:
          If the wanderer busies himself with trivial things,
          He draws down misfortune upon himself.

          Six in the second place means:
          The wanderer comes to an inn.
          He has his property with him.
          He wins the steadfastness of a young servant.

          Nine in the third place means:
          The wanderer's inn burns down.
          He loses the steadfastness of his young servant.
          Danger.

          Nine in the fourth place means:
          The wanderer rests in a shelter.
          He obtains his property and an ax.
          My heart is not glad.

     ()   Six in the fifth place means:
          He shoots a pheasant.
          It drops with the first arrow.
          In the end this brings both praise and office.

          Nine at the top means:
          The bird's nest burns up.
          The wanderer laughs at first,
          Then must needs lament and weep.
          Through carelessness he loses his cow.
          Misfortune.
"""
verse57 = """
  57 ䷸ The Gentle (The Penetrating, Wind)

     The Judgement

          The Gentle. Success through what is small.
          It furthers one to have somewhere to go.
          It furthers one to see the great man.

     The Image

          Winds following one upon the other:
          The image of the Gently Penetrating.
          Thus the superior man
          Spreads his commands abroad
          And carries out his undertakings.

     The Lines

     []   Six at the beginning means:
          In advancing and in retreating,
          The perseverance of a warrior furthers.

          Nine in the second place means:
          Penetration under the bed.
          Priests and magicians are used in great number.
          Good fortune. No blame.

          Nine in the third place means:
          Repeated penetration. Humiliation.

     []   Six in the fourth place means:
          Remorse vanishes.
          During the hunt
          Three kinds of game are caught.

     ()   Nine in the fifth place means:
          Perseverance brings good fortune.
          Remorse vanishes.
          Nothing that does not further.
          No beginning, but an end.
          Before the change, three days.
          After the change, three days.
          Good fortune.
"""
verse58 = """
  58 ䷹ The Joyous, Lake

     The Judgement

          The Joyous. Success.
          Perseverance is favorable.

     The Image

          Lakes resting one on the other:
          The image of the Joyous.
          Thus the superior man joins with his friends
          For discussion and practice.

     The Lines

          Nine at the beginning means:
          Contented joyousness. Good fortune.

     ()   Nine in the second place means:
          Sincere joyousness. Good fortune.
          Remorse disappears.

     []   Six in the third place means:
          Coming joyousness. Misfortune.

          Nine in the fourth place means:
          Joyousness that is weighed is not at peace.
          After ridding himself of mistakes a man has joy.

     ()   Nine in the fifth place means:
          Sincerity toward disintegrating influences is dangerous.

     []   Six at the top means:
          Seductive joyousness."""
verse59 = """
  59 ䷺ Dispersion [Dissolution]

     The Judgement

          Dispersion. Success.
          The king approaches his temple.
          It furthers one to cross the great water.
          Perseverance furthers.

     The Image

          The wind drives over the water:
          The image of Dispersion.
          Thus the kings of old sacrificed to the Lord
          And built temples.

     The Lines

          Six at the beginning means:
          He brings help with the strength of a horse.
          Good fortune.

     []   Nine in the second place means:
          At the dissolution
          He hurries to that which supports him.
          Remorse disappears.

          Six in the third place means:
          He dissolves his self. No remorse.

     []   Six in the fourth place means:
          He dissolves his bond with his group.
          Supreme good fortune.
          Dispersion leads in turn to accumulation.
          This is something that ordinary men do not think of.

     ()   Nine in the fifth place means:
          His loud cries are as dissolving as sweat.
          Dissolution. A king abides without blame.

          Nine at the top means:
          He dissolves his blood.
          Departing, keeping at a distance, going out,
          Is without blame."""
verse60 = """
  60 ䷻ Limitation

     The Judgment

          Limitation. Success.
          Galling limitation must not be persevered in.

     The Image

          Water over lake: the image of LIMITATION.
          Thus the superior man
          Creates number and measure,
          And examines the nature of virtue and correct conduct.

     The Lines

          Nine at the beginning means:
          Not going out of the door and the courtyard
          Is without blame.

          Nine in the second place means:
          Not going out of the gate and the courtyard
          Brings misfortune.

          Six in the third place means:
          He who knows limitation
          Will have cause to lament.
          No blame.

          Six in the fourth place means:
          Contented limitation. Success.

          ° Nine in the fifth place means:
          Sweet limitation brings good fortune.
          Going brings esteem.

          Six at the top means:
          Galling limitation.
          Perseverance brings misfortune.
          Remorse disappears.
"""
verse61 = """
  61 ䷼ Inner Truth

     The Judgment

          Inner Truth. Pigs and fishes.
          Good fortune.
          It furthers one to cross the great water.
          Perseverance furthers.

     The Image

          Wind over lake: the image of Inner Truth.
          Thus the superior man discusses criminal cases
          In order to delay executions.

     The Lines

          Nine at the beginning means:
          Being prepared brings good fortune.
          If there are secret designs, it is disquieting.

          Nine in the second place means:
          A crane calling in the shade.
          Its young answers it.
          I have a good goblet.
          I will share it with you.

          Six in the third place means:
          He finds a comrade.
          Now he beats the drum, now he stops.
          Now he sobs, now he sings.

          Six in the fourth place means:
          The moon nearly at the full.
          The team horse goes astray.
          No blame.

          ° Nine in the fifth place means:
          He possesses truth, which links together.
          No blame.

          Nine at the top means:
          Cockcrow penetrating to heaven.
          Perseverance brings misfortune.
"""
verse62 = """
  62 ䷽ Preponderance of the Small

     The Judgment

          Preponderance Of The Small. Success.
          Perseverance furthers.
          Small things may be done; great things should not be done.
          The flying bird brings the message:
          It is not well to strive upward,
          It is well to remain below.
          Great good fortune.

     The Image

          Thunder on the mountain:
          The image of Preponderance Of The Small.
          Thus in his conduct the superior man gives preponderance to reverence.
          In bereavement he gives preponderance to grief.
          In his expenditures he gives preponderance to thrift.

     The Lines

          Six at the beginning means:
          The bird meets with misfortune through flying.

          ° Six in the second place means:
          She passes by her ancestor
          And meets her ancestress.
          He does not reach his prince
          And meets the official.
          No blame.

          Nine in the third place means:
          If one is not extremely careful,
          Somebody may come up from behind and strike him.
          Misfortune.

          Nine in the fourth place means:
          No blame. He meets him without passing by.
          Going brings danger. One must be on guard.
          Do not act. Be constantly persevering.

          ° Six in the fifth place means:
          Dense clouds,
          No rain from our western territory.
          The prince shoots and hits him who is in the cave.

          Six at the top means:
          He passes him by, not meeting him.
          The flying bird leaves him.
          Misfortune.
          This means bad luck and injury.
"""
verse63 = """
  63 ䷾ After Completion

     The Judgment

          After Completion. Success in small matters.
          Perseverance furthers.
          At the beginning good fortune.
          At the end disorder.

     The Image

          Water over fire: the Image Of The Condition
          In After Completion.
          Thus the superior man
          Takes thought of misfortune
          And arms himself against it in advance.

     The Lines

          Nine at the beginning means:
          He breaks his wheels.
          He gets his tail in the water.
          No blame.

          ° Six in the second place means:
          The woman loses the curtain of her carriage.
          Do not run after it;
          On the seventh day you will get it.

          Nine in the third place means:
          The Illustrious Ancestor
          Disciplines the Devil's Country.
          After three years he conquers it.
          Inferior people must not be employed.

          Six in the fourth place means:
          The finest clothes turn to rags.
          Be careful all day long.

          Nine in the fifth place means:
          The neighbor in the east who slaughters an ox
          Does not attain as much real happiness
          As the neighbor in the west
          With his small offering.

          Six at the top means:
          He gets his head in the water. Danger.
"""
verse64 = """
  64 ䷿ Before Completion

     The Judgment

          BEFORE COMPLETION. Success.
          But if the little fox, after nearly completing the crossing,
          Gets his tail in the water,
          There is nothing that would further.

     The Image

          Fire over water:
          The image of the condition before transition.
          Thus the superior man is careful
          In the differentiation of things,
          So that each finds its place.

     The Lines

          Six at the beginning means:
          He gets his tail in the water.
          Humiliating.

          Nine in the second place means:
          He brakes his wheels.
          Perseverance brings good fortune.

          Six in the third place means:
          Before completion, attack brings misfortune.
          It furthers one to cross the great water.

          Nine in the fourth place means:
          Perseverance brings good fortune.
          Remorse disappears.
          Shock, thus to discipline the Devil's Country.
          For three years, great realms are rewarded.

          ° Six in the fifth place means:
          Perseverance brings good fortune.
          No remorse.
          The light of the superior man is true.
          Good fortune.

          Nine at the top means:
          There is drinking of wine
          In genuine confidence. No blame.
          But if one wets his head,
          He loses it, in truth.
"""

verses = [
  verse01, verse02, verse03, verse04, verse05, verse06, verse07, verse08,
  verse09, verse10, verse11, verse12, verse13, verse14, verse15, verse16,
  verse17, verse18, verse19, verse20, verse21, verse22, verse23, verse24,
  verse25, verse26, verse27, verse28, verse29, verse30, verse31, verse32,
  verse33, verse34, verse35, verse36, verse37, verse38, verse39, verse40,
  verse41, verse42, verse43, verse44, verse45, verse46, verse47, verse48,
  verse49, verse50, verse51, verse52, verse53, verse54, verse55, verse56,
  verse57, verse58, verse59, verse60, verse61, verse62, verse63, verse64
  ]

from subprocess import call
if __name__ == "__main__":
    random.seed()
    call("clear")
    print(verses[random.randint(0, 63)])

    lines = []
    for i in range(6):
        coin1 = random.randint(2, 3)
        coin2 = random.randint(2, 3)
        coin3 = random.randint(2, 3)
        lines.append(coin1 + coin2 + coin3)
    print(lines)
