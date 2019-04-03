# Interview Anybuddy
Créer un service web qui accepte des événements de type iCalendar et les écrits dans une base de données.


## Objectifs
- [x] Modéliser l'event VEVENT en case class
- [x] Accepter un événement de type VEVENT (prendre l’exemple page 53 du RFC5545)
      via un web service http
- [ ] Parser l'événement
- [ ] Enregistrer l'événement dans une base de données (Potsgres)
- [ ] Écrire les tests
      
## Contraintes
- [x] Langage scala
- [x] Utilisation de Git

## Lancement du projet
#### Clonage du projet
```git clone git@github.com:Rlucas12/interview_anybuddy.git```  


#### Création de la BDD PostgreSQL via docker
```docker run --name anybuddy -e POSTGRES_USER=root -e POSTGRES_PASSWORD=root -e POSTGRES_DB=anybuddy -it -p 5433:5432 -d postgres```



## Tester le projet
````sbt test````