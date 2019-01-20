# DisneyWorld FastPassPlus Booking Agent

This application uses browser automation to select FastPass from DisneyWorld (Orlando) FastPassPlus system.
It does not work with DisneyLand (LA) MaxPass system (yet).
Popular rides like Avatar can be obtained consistently, which is impossible with manual selection. 
## Get Started

1. Download the executable jar from Sonatype with the most recent version:
https://oss.sonatype.org/content/repositories/releases/org/datacrafts/dwfpp_2.12/1.0.2/dwfpp_2.12-1.0.2-assembly.jar
 
2. Download chromedriver OS specific executable from (http://chromedriver.chromium.org).
Chrome must be installed independently, and chromedriver must be compatible with the installed Chrome.
3. Copy paste the application.conf file and modify it for choice of theme park, date and guests
4. Install Java runtime 
5. Run the following command with all three of the above mentioned files under the current directory:
downloaded jar from step 1, chromedriver executable from step 2, and application.conf from step 3.
```text
java -jar <the jar file name from step 1> 
``` 

The agent will run forever trying to improve the FastPass selection. Use Control-C to kill it.

## Configuration
The application.conf file is in Human-Optimized-Config-Object-Notation(HOCON) format.
The syntax is intuitive enough and mostly self-explained (https://en.wikipedia.org/wiki/HOCON).
The template configuration is already setup with most of the information and reasonable defaults.
You just need to customize the following information:

Disney account login, guest names and date of selection are at the top of the application.conf file.
```hocon
// login information to
// https://disneyworld.disney.go.com/login/
login {
  userName = ""
  password = ""
}

// names of the guests in the party, as they appear in the account page firstName lastName
guests = [""]

// the target date
// since only upto 60 days is allowed, month and day is enough
date = {
  month = January
  day = 14
}

```

Theme park selection is embedded in the detailed park configuration.<BR>
Set selected to true to select the specific park, only 1 park can be selected.<BR>
Each park has multiple lands and attractions inside the lands, and each attraction has a numeric value.
The FastPass booking agent will use a greedy algorithm to select from the available FastPasses with the highest value.
HourPreferences attribute can be optional set for each attraction (otherwise use the global one), which will be used as additional multiplying factor based on
interpolated value. The default setting gives 11am highest value.

```hocon
parks: [
  {
    name: "Animal Kingdom"
    selected: true // this park is selected, can only select 1
    groups: [
      {
        name: tier1
        limit = 1
      }
    ]
    lands: [
      {
        name: Pandora
        attractions: [
          {
            name: "Na'vi River"
            value: 100
            group: tier1,
            modifyCycles: 100 // modify to get Avatar flight, more cycles means more chance
          }
          {
            name: "Avatar Flight"
            value: 1000
            group: tier1
          }
        ]
      }

      {
        name: Africa
        attractions: [
          {
            name: Safari
            value: 50
            hourPreferences: [
              {
                hour = 9
                value = 1
              }
              {
                hour = 10
                value = 1.5
              }
              {
                hour = 11
                value = 2
              }
              {
                hour = 17
                value = 1
              }
              {
                hour = 17.5
                value = 0 // may not want to go to safari after darkness
              }
            ]
          }
        ]
      }

      {
        name: DinoLand
        attractions: [
          {
            name: DINOSAUR
            value: 1
          }
          {
            name: Whirl
            value: 1
          }
        ]
      }

      {
        name: Asia
        attractions: [
          {
            name: "Forbidden Mountain"
            value = 10
          }
          {
            name: "Kali River"
            value = 5
          }
        ]
      }
    ]
  }

  {
    name: Epcot
    selected: false
    groups: [
      {
        name: tier1
        limit = 1
      }
    ]
    lands: [
      {
        name: "World Showcase"
        attractions: [
          {
            name: Frozen
            value: 5
            group: tier1
          }
          {
            name: IllumiNations
            value: 1
            group: tier1
          }
        ]
      }

      {
        name: "Future World"
        attractions: [
          {
            name: Soarin
            value = 20
            group = tier1
          }
          {
            name: "Test Track"
            value = 20
            group = tier1
          }
          {
            name: "Meet Disney Pals"
            value = 1
            group = tier1
          }

          {
            name: "Mission"
            value = 20
          }

          {
            name: Spaceship
            value = 5
          }

          {
            name: "Living with the Land"
            value = 1
          }
        ]
      }
    ]
  }

  {
    name: "Magic Kingdom"
  }

  {
    name: "Hollywood Studios"
  }
]
```
## Specific Behavior
The agent will first select the guests, date and park.
Select a higher valued FastPass from the specified park may lead to cancelling all the existing FastPasses from another park.
When all 3 FastPasses are filled, the agent will randomly modify existing FastPasses.
Unspecified attractions or attractions failed to match with the config have 0 value and will not be considered.
Attraction matching is based on name string and is case insensitive. For example the full name for Soarin is "Soarin' Around the World".
The config is fine as long as it's contained in the full name.
This rule applies to park name matching as well.
Animal Kingdom and Epcot both have tiered attractions that only allow 1 FastPass in the group.
This information is useful for the agent to pick the right move, since it need to understand the consequence of each selection
before clicking the link.
It sleeps for 10 seconds for every refresh so that it won't create too much traffic load - be a good bot.

Raise an issue [here](../../../issues) if you have any questions.
