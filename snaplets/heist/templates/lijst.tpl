<apply template="base">

  <ifLoggedIn>
    <table border="1">
        <caption>Schuldenaars</caption>
        <tr>
          <th>Naam</th>
          <th>Schuld</th>
        </tr>
      <schuld>
        <tr>
          <td><name/></td>
          <td><debt/></td>
        </tr>
      </schuld>
    </table>
  <p><a href="/boodschap">Nieuw boodschapke gedaan?</a></p>
  </ifLoggedIn>

</apply>
