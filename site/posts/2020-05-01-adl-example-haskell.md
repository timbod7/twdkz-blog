---
title: "Implementing an ADL API in haskell"
author: Tim Docker
date: 2020-05-01
tags: [programming]
---

# Introduction

This is the second post in a series where we use [ADL][] to build a
multi-language system with consistent types. In the [first post][post1]
we wrote the specification for the API. In this post we will implement
a server for the API in haskell. This post presents key snippets of the
server code - follow the links to the [source code repo][adldemo] to see these in
context.

# Our tools

We'll keep our code small and leverage the haskell ecosystem by making
use of the following libraries:

* The [Spock][] web framework
* [Data.Password][] for secure password management
* [Web.JWT][] for Json Web Token functions

# The code structure

For reference, the project code structure is as below. There are also the usual files
to support stack and cabal.

| File                                                                        | Description                         |
| :-                                                                          | :-                                  |
| [`messageboard-api/adl/*`][mbadl]                                           | the ADL definitions                 |
| [`messageboard-api/scripts/generate-adl.sh`][generate-adl.sh]               | script to generate code from ADL    |
| [`messageboard-api/haskell/src/ADL/*`][mbadlhs]                             | haskell code generated from the ADL | 
| [`messageboard-api/haskell/src/Main.hs`][Main.hs]                           | startup and config parsing          | 
| [`messageboard-api/haskell/src/Server.hs`][Server.hs]                       | the server implementation           | 
| [`messageboard-api/haskell/src/Utils.hs`][Utils.hs]                         | server helper functions             | 
| [`messageboard-api/haskell/server-config.yaml`][server-config.yaml]         | a server config file for testing    | 

[mbadlhs]:https://github.com/timbod7/adl-demo/tree/master/messageboard-api/haskell/src/ADL
[mbadl]:https://github.com/timbod7/adl-demo/tree/master/messageboard-api/adl
[generate-adl.sh]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/scripts/generate-adl.sh
[Server.hs]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/Server.hs
[Main.hs]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/Main.hs
[Utils.hs]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/Utils.hs
[server-config.yaml]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/server-config.yaml

# Configuration and scaffolding

There's not much to do here. Our server `main` loads configuration,
creates initial state, and launches spock. As described
[previously][config-post], by defining our configuration in ADL:

```
struct ServerConfig {

  /// The port which accepts http connections
  Int32 port = 8080;

  /// The secret used to sign the server's json web tokens
  String jwtSecret;
};
```

we can use the ADL generated haskell code to validate and parse a YAML
config file into a well typed [haskell value][config-hs].

Loading the configuration is really the only point of interest in the
scaffolding. After than, we just have to create our initial application
state, and then launch spock:

```
main :: IO ()
main = do
  args <- getArgs
  case args of
    [configPath] -> do
      eConfig <- adlFromYamlFile configPath
      case eConfig of
        (Left emsg) -> exitWithError (T.unpack emsg)
        (Right config) -> startServer config
    _ -> exitWithError "Usage: server <config.yaml>"
  
startServer :: ServerConfig -> IO ()
startServer sc = do
  state <- initAppState sc
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase state
  runSpock (fromIntegral (sc_port sc)) (spock spockCfg serverApp)
```

(see [Main.hs](https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/Main.hs#L29))

# Our server structure

We are using the ADL API definition discussed in the [previous
post][post1].  For the purpose of this example, we will keep the application
state in server memory and use haskell [STM] to manage concurrent access.
(In a future post I'll show how we can implement a persistence layer that
leverages ADL to define the persisted data model). Our application needs
to maintain a list of the users allows to login, and the messages that
have been sent. Here's the core state declaration:

```
data User = User {
  u_email :: T.Text,
  u_hashedPassword :: T.Text,
  u_isAdmin :: Bool
}

data MyAppState = MyAppState {
  mas_serverConfig :: ServerConfig,
  mas_users:: TVar [User],             -- the users that can login
  mas_messages:: TVar [API.Message]    -- the messages that have been posted
}
```
(see [Server.hs](https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/Server.hs#L29))

Our spock endpoint handlers will have a somewhat intimidating return type:

```
type MyHandler o = ActionCtxT MyContext (WebStateM () MySession MyAppState) o
```

I recommend reading the spock documentation to understand this in detail, but
in the context of this post, it's enough to know that `MyHandler` is a
Monad within which one can

* use `liftIO` to run `IO` actions.
* use `getState` to access the `MyAppState` value

Let's delve into to the details of the login API endpoint. It has the following
ADL definition:

```
  HttpPost<LoginReq,LoginResp> login = {
    "path" : "/login",
    "security" : "public"
  };

struct LoginReq {
  Email email;
  String password;
};

union LoginResp {
  Jwt success;
  Void failure;
};
```

which, thanks to the ADL compiler, results in haskell definitions for [LoginReq][],
[LoginResp][], and the http request [metadata][].

So our login handler will have the following signature:

```
handleLogin :: API.LoginReq -> MyHandler API.LoginResp
```

We will write a helper function `adlPost` that, given the appropriate
`HttpPost<I,O>` metadata connects our handler to the spock server. By "connects"
I mean that it will:

- route post requests with the declared path
- check authentication
- deserialize and validate the post request body into the appropriate `I` value
- call our handler implementation
- serialize the `O` value, and send it as the post response body.

The `adlPost` helper function will have the following signature:

```
adlPost :: (AdlValue i, AdlValue o)
        => HttpPost i o
        -> (i -> MyHandler o)
        -> SpockCtxM ctx conn sess MyAppState ()
```

(The actual implementation will have a slightly more general type to avoid
dependence on `MyAppState` - see below).

This helper function makes implement the spock API very easy. Our spock server
is implemented simply by connecting each handler:

```
serverApp :: SpockM () MySession MyAppState ()
serverApp = do
  let api = API.mkApi
  adlPost (API.api_login api) handleLogin
  adlPost (API.api_newMessage api) handleNewMessage
  adlPost (API.api_recentMessages api) handleRecentMessages
  adlPost (API.api_createUser api) handleCreateUser
  adlPost (API.api_ping api) handlePing
```
(see [Server.hs](https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/Server.hs#L57))


with each handler having the expected, strongly typed signature:

```
handleLogin :: API.LoginReq -> MyHandler API.LoginResp
handleNewMessage :: API.NewMessageReq -> MyHandler Empty
handleRecentMessages :: API.RecentMessagesReq -> MyHandler [API.Message]
handleCreateUser :: API.CreateUserReq -> MyHandler API.CreateUserResp
```

# Implementing `adlPost`

As described above, the `adlPost` function will deal with the endpoint routing, authentication,
validation and serialization, ie pretty much all of the boilerplate code typically required
for an endpoint. Whilst it has quite a lot to do, it's relatively concise - lets
show the code in full here:

```
-- | Add a spock route implementing an http post request, with the specification for
-- the request supplied as a value of type HttpPost.
--
-- Assuming a request body of type i, and a response body of type o, the resulting
-- handler implements JWT based authorization checks, and request and response parsing
-- and serialization.
adlPost :: (AdlValue i, AdlValue o, HasJwtSecret st)
        => HttpPost i o
        -> (i -> ActionCtxT (Maybe JWTClaimsSet) (WebStateM conn sess st) o)
        -> SpockCtxM ctx conn sess st ()
adlPost postmeta handler = prehook checkAuth $ post path runRequest
  where
    path = fromString (T.unpack (hp_path postmeta))

    checkAuth = do
      jwtSecret <- getJwtSecret <$> getState
      case hp_security postmeta of
        HS_public -> return Nothing
        HS_token -> Just <$> getVerifiedJwtClaims jwtSecret
        HS_adminToken -> do
          claims <- getVerifiedJwtClaims jwtSecret
          when (not (isAdmin claims)) $ do
            error401 "needs admin"
          return (Just claims)

    runRequest = do
      mjv <- jsonBody
      case mjv of
        Nothing -> error400 "json body not well formed"
        (Just jv) -> do
          let pv = runJsonParser jsonParser [] jv
          case decodeAdlParseResult " from post body " pv of
            Left e -> error400 e
            Right i -> do
              o <- handler i
              json (adlToJson o)
```

(see [Utils.hs](https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/Utils.hs#L36))

It takes two parameters: `postmeta` is metadata describing the post
request, and `handler` is the application handler function. The request
and response bodies (type `i` and `o`) must be ADL values, (which
they will be given that the postmeta value was generated by the ADL
compiler). Our type signature is generalized from that show previously
in that it can work with any spock state (type `st`) provided that we
have a means of extracting a jwt secret from that state.  This secret
is needed to validate JWTs and hence check authorization.

It return a monadic value of type `SpockCtxM` which we used above to
actually create the spock handler.

`adlPost` works in two phases - it runs `checkAuth` as a [spock
prehook][prehook], and then runs the request as a [spock post action][post].

`checkAuth` performs case analysis to ensure that the incoming
request meets the security requirements for the endpoint as per the
[api spec][api-def]. If the endpoint is public there is no check to
perform. If the endpoint requires a token, we verify that the request
has a correctly signed Json Web Token. If the endpoint requires an admin
token, we also verify that the valid JWT has an `isAdmin` claim. The
prehook returns the JWT, which hence becomes the spock request context.
This context is accessible in request handlers.

Assuming that we pass the authorization checks, `runRequest` 

* extracts the post request body as json
* parses the json into a value of type `i`
* calls the application handler
* serializes the result of type `o` into json
* sends that response back to the API client (with a response code of 200)

If either of the first two steps fails, a bad request (400) response code will
result.

Whew! Quite a lot of explanatory text for a small function. But it's a
tribute to haskell's expressiveness that we can write a function sufficiently
abstract that that it implements the API boilerplate for our whole API.

[prehook]:http://hackage.haskell.org/package/Spock-core-0.13.0.0/docs/Web-Spock-Core.html#v:prehook
[post]:http://hackage.haskell.org/package/Spock-core-0.13.0.0/docs/Web-Spock-Core.html#v:post

# Implementing the application logic

Whilst the main goal for this post was to demonstrate ADL API definitions, let's
complete the server by fleshing out the API application logic. We've got 4 methods
to implement:

## `handleLogin :: API.LoginReq -> MyHandler API.LoginResp`

The login endpoint needs to

* verify that a user with the given email address exists
* verify that the password supplied matches the stored scrypt hash
* construct a JWT for the user that embeds the email address and login

The JWT ([JSON Web Token][JWT]) is returned to the client, and is subequently
provided to the server as proof that a login has succeeded.

See [Server.handleLogin][handleLogin] for the implementation code.

## `handleNewMessage :: API.NewMessageReq -> MyHandler Empty`

The new message endpoint simply accepts message text from the client,
and appends it and some metadata to the message list in the server state. The
implementation accesses the spock request context to recover the JWT
(already validated by `postAdl`), in order to determine the email of
the user posting the message.

See [Server.handleNewMessage][handleNewMessage] for the implementation code.

## `handleRecentMessages :: API.RecentMessagesReq -> MyHandler [API.Message]`

This endpoint is trivial - the handler just needs to extract the requested
number of messages from the application state, and return them to the client.

See [Server.handleRecentMessages][handleRecentMessages] for the implementation code.

## `handleCreateUser :: API.CreateUserReq -> MyHandler API.CreateUserResp`

In our application, only admin users are authorized to create new users, but
that is specified in the API definition, and hence is checked before the handler
is called. The handler must:

* verify that there is not an existing user with the requested email address, and
  if this is the case, indicate it to the client.
* hash the provided password, and add the new user to the application state.

See [Server.handleCreateUser][handleCreateUser] for the implementation code.

# Testing

If you've checked out the [project source code][adldemo], you can build and run the
server with stack:

```
$ cd messageboard-api/haskell
$ stack run messageboard-server server-config.yaml

spock is running on port 8080
```

Whilst we plan to build a strongly typed client for the API, we can
test it now via curl. For demo purposes the initial app state includes a
test user. Let's try issuing a post login request with an empty body:

```
$ curl http://localhost:8080/login -d '{}'
Unable to parse a value of type api.LoginReq from post body : expected field email at $
```

OK - the 400 error tells us what is wrong with our request. Let's fill it in correctly
 with the test user's details (as per the ADL `LoginReq` type):

```
$ curl http://localhost:8080/login -d '{
  "email": "admin@test.com",
  "password": "xyzzy"
}'

{"success":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImFkbWluQHRlc3QuY29tIiwiYWRtaW4iOnRydWV9.1mZfzhRO_hubbFI2LNBj7wnYUwThTMlSfVaawenX33Y"}$ 
```

Success. We now have a JWT for future requests as the initial test user. Put it in
a shell variable, and let's see if there are any messages:

```
$ JWT=...token...
$ curl http://localhost:8080/recent-messages -H "Authorization:Bearer $JWT" -d '{
  "maxMessages": 10
}'

[]
```

No. So let's post a few:

```
$ curl http://localhost:8080/new-message -H "Authorization:Bearer $JWT" -d '{
  "body": "First post!"
}'

{}
$ curl http://localhost:8080/new-message -H "Authorization:Bearer $JWT" -d '{
  "body": "and a followup"
}'

{}
```
... and check that we can fetch them (using [jq][] to tidy up the formatting):

```
$ curl -s http://localhost:8080/recent-messages -H "Authorization:Bearer $JWT" -d '{
  "maxMessages": 10
}' | jq .

[
  {
    "body": "and a followup",
    "postedAt": "2020-05-04T09:32:11.258139377",
    "postedBy": "admin@test.com",
    "id": "2"
  },
  {
    "body": "First post!",
    "postedAt": "2020-05-04T09:31:04.024827574",
    "postedBy": "admin@test.com",
    "id": "1"
  }
]
```

Finally, let's create a new user, and excercise the API as that user:

```
$ curl -s http://localhost:8080/create-user -H "Authorization:Bearer $JWT" -d '{
  "email": "user@test.com",
  "password": "notmuchofapassword",
  "isAdmin": false
}'

{"success":"2"}

$ curl http://localhost:8080/login -d '{
  "email": "user@test.com",
  "password": "notmuchofapassword"
}'

{"success":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6InVzZXJAdGVzdC5jb20iLCJhZG1pbiI6ZmFsc2V9.48FYSck2FwaBwQgwhBIiQVH7ks5rmcvcPmSwoEpBZ6E"}

$ JWT2=...token...
$ curl http://localhost:8080/new-message -H "Authorization:Bearer $JWT2" -d '{
  "body": "Greetings!"
}'

{}

$ curl -s http://localhost:8080/recent-messages -H "Authorization:Bearer $JWT2" -d '{
  "maxMessages": 10
}' | jq .

[
  {
    "body": "Greetings!",
    "postedAt": "2020-05-04T09:45:16.443301183",
    "postedBy": "user@test.com",
    "id": "3"
  },
  {
    "body": "and a followup",
    "postedAt": "2020-05-04T09:32:11.258139377",
    "postedBy": "admin@test.com",
    "id": "2"
  },
  {
    "body": "First post!",
    "postedAt": "2020-05-04T09:31:04.024827574",
    "postedBy": "admin@test.com",
    "id": "1"
  }
]
```

# Summing up

With only a small amount of code, we have implemented our API in haskell, and abstracted
out all of the boilerplate code associated with:

* de/serialization
* validation
* authorization

leaving us to implement the application logic in a strongly typed framework. Hopefully the
utility of using ADL to specify the API and associated data types is apparent. ADL's value
increases with a more realistic project where:

* multiple languages are involved
* the API grows, with more endpoints and more complex data types
* the API evolves over time

In my next post, I will demonstrate how we can build a typescript client for this API.

Feel free to post questions and comments as issues on the [project repo][adldemo].

[adldemo]:https://github.com/timbod7/adl-demo
[LoginReq]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/ADL/Api.hs#L103
[LoginResp]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/ADL/Api.hs#L124
[metadata]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/ADL/Api.hs#L47


[ADL]:https://github.com/timbod7/adl
[post1]:/posts/2020-04-24-adl-example-api.html
[Spock]:https://hackage.haskell.org/package/Spock
[Data.Password]:https://hackage.haskell.org/package/password-1.0.0.0
[Web.JWT]:https://hackage.haskell.org/package/jwt-0.10.0
[STM]:https://hackage.haskell.org/package/stm
[server-main]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/Main.hs#L28
[config-post]:/posts/2020-05-01-adl-example-haskell.html
[config-schema]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/adl/config.adl
[config-hs]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/ADL/Config.hs#L17
[api-def]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/adl/api.adl
[JWT]:https://jwt.io/introduction/
[handleLogin]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/Server.hs#L69
[handleNewMessage]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/Server.hs#L94
[handleRecentMessages]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/Server.hs#L110 
[handleCreateUser]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/haskell/src/Server.hs#L117
[jq]:https://stedolan.github.io/jq/
