# elm-resources

## A collection of Elm Resources

## [https://lucamug.github.io/elm-resources/](https://lucamug.github.io/elm-resources/)

[![Screenshot](https://lucamug.github.io/elm-resources/img/elm-resources.png)](https://lucamug.github.io/elm-resources/)

Feel free to add resources sending Pull Requests or opening Issues.

## How to add a link

Open the file `src/data/Links.elm` and add a new record in the list inside the `list` function.

The type of the record is:
```
type alias Attributes =
    { name : String               -- Name of the link, also used as ID
    , url : String                -- Url of the link
    , code : String               -- Url to the source code, if available. Otherwise empty string.
    , picture : String            -- Picture representing the link, if available. Otherwise empty string.
    , description : String        -- Description of the link
    , keywords : List Keywords.Id -- List of related Keywords, from `src/data/Keywords.elm`
    , authors : List People.Id    -- List of related Authors, from `src/data/People.elm`
    }
```

## How to add a Keyword

Open the file `src/data/Keywords.elm` and add

* a new constructor for the type `Id`
* a new record in the list inside the `list` function.

The type of the record is:
```
type alias Attributes =
    { id : Id          -- The type constructor
    , name : String    -- Name, usually the same as the type constructor
    , picture : String -- Picture that represent the keyword
    }
```

## How to add a Person

Open the file `src/data/People.elm` and add

* a new constructor for the type `Id`
* a new record in the list inside the `list` function.

The type of the record is:
```
type alias Attributes =
    { id : Id          -- The type constructor
    , name : String    -- Name, usually the same as the type constructor
    , picture : String -- Picture that represent the person
    , twitter : String -- Twitter handle
    , github : String  -- Github handle
    , url : String     -- Link to a personal homepage
    }
```

## Development

To run the app

```
npm install
npm start
```

## Production

To build the app

```
cmd/build/start
```
