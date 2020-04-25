---
title: "API specifications in ADL"
author: Tim Docker
date: 2020-04-24
tags: [programming]
---

# Introduction

This post is the first of a series where I will demonstrate using the
[ADL][adl] system to specify an HTTP based API, and implement conforming
servers and clients in different programming languages.

In this post, I will explore how ADL can be used to specify APIs,
and do this for a simple application. The API will be small enough for
demonstration purposes, but will include login, authorization, and basic
application functions.

Future posts will implement servers for this API in haskell and rust, and an
API client in typescript (for use in the browser). The ADL type definitions
will "glue" the multi-language system together, ensuring consistent static types
between languages. Hence ADL's mantra:

> Consistent types everywhere!

# Why not use ...?

In this post we are using ADL to define a API as one would with other
API definition languages such as [openapi][], [grpc][] and similar tools.
ADL has some key benefits compared with such tools including:

* parameterized types (aka generics)
* [custom type mappings][customtypes]
* general purpose [annotations][]

More importantly ADL differs in that it is intended as a general purpose
tool for data modelling. Here we are using in to specify an API, but
it also appropriate for other purposes (eg specifying relational data
models, automatically generated forms, [type checked configuration
files][post-adl-configs], etc)

# Our application and it's API

Our sample application is somewhat of a cliche: a multi-user message
board. It will have the following features:

* Users must login to access the application
* Once logged in, users can view recent messages and post new messages
* Certain users will have "admin" privileges and they are able to create new users.

Our API will be implemented conventionally: as JSON messages passed
over HTTP. Given a specification of the API in ADL, the ADL compiler
will be used to translate that specification into types and values
in our programming languages of choice (here: haskell, rust and
typescript). Then, in each of those programming languages we will
write generic library code to interpret that specification and
implement the boilerplate associated with serialization, validation,
and authorization. We will be left to implement just the application
logic itself.

ADL doesn't have any baked in knowledge of the HTTP protocol. So
we must start by declaring a data type that captures our specification
for an HTTP request. In our simplified API, all requests will be HTTP
post requests. If one desired a more "[RESTy][rest]" api then there
would be similar definitions for the other HTTP methods.

```
// A post request with request body of type I, and response
// body of type O
struct HttpPost<I,O> {
  String path;
  HttpSecurity security;
  TypeToken<I> reqType = null;
  TypeToken<O> respType = null;
};

union HttpSecurity {
  // The endpoint is publically accessible
  Void public;

  // A JWT is required in a bearer authoration header
  Void token;

  // A JWT with an admin claim is required in a bearer authoration header
  Void adminToken;
};
```

Let's pull this definition apart. For each API request we can make we
need to specify:

* the type of the request body sent to the server: `I`
* the type of the response returned to the client: `O`
* the http path for this request
* the authorization rules for this endpoint.

As per the subsequent `HttpSecurity` definition, in our simple security model
API endpoints can be public, or require a token (proving that a user has
logged in), or requiring an admin token (proving that a user has logged
in and has admin rights).

The `HttpPost` structure captures all this information as a runtime
value which we will interpret with library code to implement all of the
boilerplate for our the endpoints. Hence we will need access to a
runtime representation of the `I` and `O` types using the ADL `TypeToken<>`
primitive.

This all probably seems a bit abstract, so lets now use `HttpPost` to define
our first endpoint:

```
struct Api {

  HttpPost<LoginReq,LoginResp> login = {
    "path" : "/login",
    "security" : "public"
  };
  
  ...
};

struct LoginReq {
  Email email;
  String password;
};

union LoginResp {
  Jwt success;
  Void failure;
};

type Jwt = String;
type Email = String;
``` 

Our runtime inspectable API will be a value of type `Api`. This is a
struct, with a field for each request endpoint. We use the ADL [defaulting
mechanism][defaults] to specify the values associated with each endpoint.

As you can see above, the login endpoint will accept a Json serialized
value of type `LoginReq`, and return a `LoginResp` sum type value, with
a [Json Web Token][jwt] on success. It's a public endpoint, so doesn't
require authentication to call.

Let's flesh out the remaining API methods to complete our API definition:

```
struct Api {
  /// Login to obtain an authorization token
  HttpPost<LoginReq,LoginResp> login = {
    "path" : "/login",
    "security" : "public"
  };

  /// Retrieve recent messages posted to the server
  HttpPost<RecentMessagesReq,Vector<Message>> recentMessages = {
    "path" : "/recent-messages",
    "security" : "token"
  };

  /// Post a new message
  HttpPost<NewMessageReq,Empty> newMessage = {
    "path" : "/new-message",
    "security" : "token"
  };

  /// Create a new user, recording their hashed password
  HttpPost<CreateUserReq,CreateUserResp> createUser = {
    "path" : "/create-user",
    "security" : "adminToken"
  };

  /// Trivial public method to test server liveness
  HttpPost<Empty,Empty> ping = {
    "path" : "/ping",
    "security" : "public"
  };
};

...

struct NewMessageReq {
  String body;
};

struct RecentMessagesReq {
  Int32 maxMessages;
};

struct CreateUserReq {
  Email email;
  Password password;
  Bool isAdmin;
};

union CreateUserResp {
  UserId success;
  Void duplicateEmail;
};

struct Message {
  String id;
  Email postedBy;
  TimeStamp postedAt;
  String body;
};
```

Hopefully these methods should be fairly self explanatory.

The [timbod7/adl-demo][adl-demo] github repository will host the code
for this blog post series. It currently contains

* the [ADL definitions][mb-adl]
* the [script][mb-adl-gen] to do the code generation
* the generated [haskell][mb-adl-hs] and [typescript][mb-adl-ts]

Feel free to ask questions in this repo's issue tracker.

# Next...

The API is defined, my next post will implement a compliant server in haskell. My
previous post on [using ADL from haskell][using-adl-haskell] may be useful background reading.

[adl]:https://github.com/timbod7/adl
[customtypes]:https://github.com/timbod7/adl/blob/master/docs/backend-haskell.md#custom-types
[annotations]:https://github.com/timbod7/adl/blob/master/docs/language.md#annotations
[defaults]:https://github.com/timbod7/adl/blob/master/docs/language.md#default-values
[openapi]:https://swagger.io/docs/specification/about/
[grpc]:https://grpc.io/
[post-adl-configs]:https://tim.dockerz.net/posts/2019-09-17-hadl-haskell.html
[rest]:https://en.wikipedia.org/wiki/Representational_state_transfer
[jwt]:https://en.wikipedia.org/wiki/JSON_Web_Token
[adl-demo]:https://github.com/timbod7/adl-demo
[mb-adl]:https://github.com/timbod7/adl-demo/tree/master/messageboard-api/adl
[mb-adl-gen]:https://github.com/timbod7/adl-demo/blob/master/messageboard-api/scripts/generate-adl.sh
[mb-adl-hs]:https://github.com/timbod7/adl-demo/tree/master/messageboard-api/haskell/src/ADL
[mb-adl-ts]:https://github.com/timbod7/adl-demo/tree/master/messageboard-api/typescript/src/adl
[using-adl-haskell]:https://tim.dockerz.net/posts/2019-09-17-hadl-haskell.html
