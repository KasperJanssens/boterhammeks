<apply template="base">

  <ifLoggedIn>
    <p>U bent ingelogd als '<loggedInUser/>'</p>

    <p><a href="/logout">Logout</a></p>
    <p><a href="/boodschap">Vermeld een nieuwe schuld</a></p>
    <p><a href="/overview">Bekijk het overzicht van de schulden</a></p>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
